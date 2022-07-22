
#include "cessor_gate.h"
/*
 *  Callback thread
 */
queue_t*
queue_create()
{
    queue_t* ret;

    ret = (queue_t*) enif_alloc(sizeof(queue_t));
    if(ret == NULL) return NULL;

    ret->lock = NULL;
    ret->cond = NULL;
    ret->head = NULL;
    ret->tail = NULL;

    ret->lock = enif_mutex_create("queue_lock");
    if(ret->lock == NULL) goto error;

    ret->cond = enif_cond_create("queue_cond");
    if(ret->cond == NULL) goto error;

    return ret;

error:
    if(ret->lock != NULL) enif_mutex_destroy(ret->lock);
    if(ret->cond != NULL) enif_cond_destroy(ret->cond);
    if(ret != NULL) enif_free(ret);
    return NULL;
}
void
queue_destroy(queue_t* queue)
{
    ErlNifMutex* lock;
    ErlNifCond* cond;

    enif_mutex_lock(queue->lock);
    assert(queue->head == NULL && "Destroying a non-empty queue.");
    assert(queue->tail == NULL && "Destroying queue in invalid state.");

    lock = queue->lock;
    cond = queue->cond;

    queue->lock = NULL;
    queue->cond = NULL;

    enif_mutex_unlock(lock);

    enif_cond_destroy(cond);
    enif_mutex_destroy(lock);
    enif_free(queue);
}
ErlNifPid*
queue_pop(queue_t* queue)
{
    qitem_t* item;
    ErlNifPid* ret = NULL;

    enif_mutex_lock(queue->lock);

    while(queue->head == NULL)
    {
        enif_cond_wait(queue->cond, queue->lock);
    }

    item = queue->head;
    queue->head = item->next;
    item->next = NULL;

    if(queue->head == NULL)
    {
        queue->tail = NULL;
    }

    enif_mutex_unlock(queue->lock);

    ret = item->pid;
    enif_free(item);

    return ret;
}
static void*
thr_main(void* obj)
{
    state_t* state = (state_t*) obj;
    ErlNifEnv* env_msg = enif_alloc_env();
    ErlNifPid* pid;
    ERL_NIF_TERM msg;

    while((pid = queue_pop(state->queue)) != NULL)
    {
        msg = enif_make_atom(env_msg, "foo");
        enif_send(NULL, state->callback, env_msg, msg);
        enif_free(pid);
        enif_clear_env(env_msg);
    }

    return NULL;
}

/*
 *  Window setup
 */
int window_setup(state_t* state)
{
    state->win = create_main_window();

    if(state->win == NULL)
        return (-1);
    show_main_window(state->win);
    printf(" [Debug] show_main_window \n");
    register_callback_thread(
                state->win,
                state->queue,
                &from_mainWindow_thread
                );
    return 0;
}


/*
 *  Application thread
 */
int
queue_push(queue_t* queue, ErlNifPid* pid)
{
    qitem_t* item = (qitem_t*) enif_alloc(sizeof(qitem_t));
    if(item == NULL) return 0;

    item->pid = pid;
    item->next = NULL;

    enif_mutex_lock(queue->lock);

    if(queue->tail != NULL)
    {
        queue->tail->next = item;
    }

    queue->tail = item;

    if(queue->head == NULL)
    {
        queue->head = queue->tail;
    }

    enif_cond_signal(queue->cond);
    enif_mutex_unlock(queue->lock);

    return 1;
}

static void*
app_thread(void* obj)
{
    state_t* state = (state_t*) obj;
    ErlNifEnv* env_msg = enif_alloc_env();
    ErlNifPid* pid;
    ERL_NIF_TERM msg;

    state->app = qt_app_new();
    if(state->app == NULL)
        msg = enif_make_atom(env_msg, "error qt_app_new");
    else
    {
        //int x = window_setup(state);
        //printf(" [Debug] window_setup %d \n", x);
        if (window_setup(state)<0)
            msg = enif_make_atom(env_msg, "error window_setup");
        else
        {
            int execution = qt_app_exec(state->app);

            if(execution == 0)
                msg = enif_make_atom(env_msg, "exited");
            else
                msg = enif_make_atom(env_msg, "error app_thread");
        }
    }
    enif_send(NULL, state->callback, env_msg, msg);
    enif_clear_env(env_msg);
    return NULL;
}

static ERL_NIF_TERM
gate_app_run(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    state_t* state = (state_t*) enif_priv_data(env);

    state->appopts = enif_thread_opts_create("app_thread");
    if(enif_thread_create(
            "", &(state->appthread), app_thread, state, state->appopts
        ) != 0)
    {
        return enif_make_atom(env, "error thread");
    }
/*
    //void **exit_value;
    if (enif_thread_join(state->appthread,NULL) != 0)
    {
        return enif_make_atom(env, "error enif_thread_join");
    }
*/
    return enif_make_atom(env, "ok");
}
/*
 *  BIF
 */
static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    printf(" [TRACE] loading...  \n");
    state_t* state = (state_t*) enif_alloc(sizeof(state_t));
    if(state == NULL) return -1;

    state->queue = queue_create();
    if(state->queue == NULL) goto error;

    state->opts = enif_thread_opts_create("thread_opts");
    if(enif_thread_create(
            "", &(state->qthread), thr_main, state, state->opts
        ) != 0)
    {
        goto error;
    }

    state->atom_ok = enif_make_atom(env, "ok");

    *priv = (void*) state;

    return 0;

error:
    if(state->queue != NULL) queue_destroy(state->queue);
    enif_free(state->queue);
    return -1;
}

static void
unload(ErlNifEnv* env, void* priv)
{
    state_t* state = (state_t*) priv;
    void* resp;

    queue_push(state->queue, NULL);
    enif_thread_join(state->qthread, &resp);
    queue_destroy(state->queue);

    enif_thread_opts_destroy(state->opts);
    enif_free(state);
}

/*
 *  API
 */
static ERL_NIF_TERM
set_callback(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    state_t* state = (state_t*) enif_priv_data(env);
    ErlNifPid* pid = (ErlNifPid*) enif_alloc(sizeof(ErlNifPid));

    state->env = env;

    //ErlNifPid * self = enif_self( env, pid);

    if(!enif_get_local_pid(env, argv[0], pid))
    {
        return enif_make_badarg(env);
    }
    //  Set gen_server pid as callback agent
    state->callback = pid;
    return state->atom_ok;
}
void
from_mainWindow_thread(queue_t* queue, int x)
{

    if (queue == NULL)
        printf(" [Debug] error queue null ! \n");
    else
    {
        ErlNifPid* pid = (ErlNifPid*) enif_alloc(sizeof(ErlNifPid));
        queue_push(queue, pid);
    }

}

static ErlNifFunc
nif_funcs[] =
{
    {"gate_app_run", 0, gate_app_run},
    {"set_callback", 1, set_callback}
};

ERL_NIF_INIT(cessor_gate,nif_funcs,&load,NULL,NULL,&unload)
