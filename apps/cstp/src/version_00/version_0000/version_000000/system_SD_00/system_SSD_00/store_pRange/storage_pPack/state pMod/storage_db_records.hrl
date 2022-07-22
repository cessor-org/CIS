%%%----FILE mess_interface.hrl----

-record(system, {key, val}).
-record(crypto, {key, val}).
-record(domain, {index, status}).
-record(contact, 
    {
        id,         %  ID of contact    derived from identity public key        32 Octets
        sid,        %  session ID       periodic random bytes                   4 Octets
        pid,        %  pid of session   the allocated pid of a session          OS32:4 OS64:8 Octets          
        status,     %  status of contact 0=excellent, 1=very strong, 2=strong, 3=normal, 4=weak, 5=very weak                  1 Octet
        flags       %  active flags  0=trust, 1=necessary                        1 Octet
    }).
-record(in_domain, {domain, contact}).
-record(compute, {key, val}).
%%%%    session
-record(sessionID, {sID, pID}).


%%%----END FILE----