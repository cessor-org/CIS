#include "input_service.h"
#include "hidden_service_input.hpp"


INPUT_service::INPUT_service()
{
}
INPUT_service::~INPUT_service()
{
}
std::string INPUT_service::input_sec()
{
    SetStdinEcho(false);

    std::string secret;
    std::cin >> secret;

    SetStdinEcho(true);

    return secret;
}
