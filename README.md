# AWS cli wrapper for Erlang

### aws_cli
- `run(Cmd, Options)` - run a command Cmd, options, for instance, can include      credentials `#{credentials =>#{key => "Key", secret => "Secret"}}`.
- `run_sync(Cmd, Options)` - synchronous version of run.

### aws_cli_s3
- `cp/2,3` - copy one file.
- `cp_r/2,3,4` - receursive copy.
- `sync/2,3` - synchronization of folders
- `ls/1,2` - list of files
- `exist/2,3`- existance of a file 