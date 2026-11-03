-module(blog_repo).
-behaviour(kura_repo).

-export([
    otp_app/0,
    start/0,
    all/1,
    get/2,
    get_by/2,
    one/1,
    insert/1,
    insert/2,
    update/1,
    delete/1,
    insert_all/2,
    update_all/2,
    delete_all/1,
    preload/3,
    transaction/1,
    multi/1,
    query/2,
    exists/1,
    reload/2,
    insert_all/3
]).

otp_app() -> blog.

start() -> kura_repo_worker:start(?MODULE).
all(Q) -> kura_repo_worker:all(?MODULE, Q).
get(Schema, Id) -> kura_repo_worker:get(?MODULE, Schema, Id).
get_by(Schema, Clauses) -> kura_repo_worker:get_by(?MODULE, Schema, Clauses).
one(Q) -> kura_repo_worker:one(?MODULE, Q).
insert(CS) -> kura_repo_worker:insert(?MODULE, CS).
insert(CS, Opts) -> kura_repo_worker:insert(?MODULE, CS, Opts).
update(CS) -> kura_repo_worker:update(?MODULE, CS).
delete(CS) -> kura_repo_worker:delete(?MODULE, CS).
insert_all(Schema, Entries) -> kura_repo_worker:insert_all(?MODULE, Schema, Entries).
update_all(Q, Updates) -> kura_repo_worker:update_all(?MODULE, Q, Updates).
delete_all(Q) -> kura_repo_worker:delete_all(?MODULE, Q).
preload(Schema, Records, Assocs) -> kura_repo_worker:preload(?MODULE, Schema, Records, Assocs).
transaction(Fun) -> kura_repo_worker:transaction(?MODULE, Fun).
multi(Multi) -> kura_repo_worker:multi(?MODULE, Multi).
query(SQL, Params) -> kura_repo_worker:query(?MODULE, SQL, Params).
exists(Q) -> kura_repo_worker:exists(?MODULE, Q).
reload(Schema, Record) -> kura_repo_worker:reload(?MODULE, Schema, Record).
insert_all(Schema, Entries, Opts) -> kura_repo_worker:insert_all(?MODULE, Schema, Entries, Opts).
