{application, waste_client, [
    {description, "A waste client application"},
    {vsn, "0.1"},
    {applications, [kernel, stdlib]},
    {modules, [waste_client, waste_client_worker]},
    {registered, [waste_client]},
    {mod, {waste_client, []}},
    {env, [
        {pools, [
            {test_pool, [
                {size, 10},
                {max_overflow, 20}
            ], [
                {server_host, "127.0.0.1"},
                {port, 5672},
                {virtual_host_path, <<"/">>},
                {username, <<"guest">>},
                {password, <<"guest">>},
                {exchange, <<"test">>},
                {routing_key, <<"test">>},
                {service_name, test_thrift }                
            ]}
        ]}
    ]}
]}.