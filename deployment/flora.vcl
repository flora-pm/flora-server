vcl 4.1;

probe heartbeat {
    .window = 5;
    .threshold = 3;
    .url = "/heartbeat";
}

backend flora-dev1 {
    # mandatory:
    .host = "127.0.0.1";
    #optional:
    .port = "8083";
    .host_header = "dev.flora.pm";
    .connect_timeout = 2s;
    .probe = heartbeat;
}

sub vcl_backend_response {
    set beresp.ttl = 10s;
    set beresp.grace = 1h;
}
