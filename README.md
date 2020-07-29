# git-post-receive

Haskellish post-receive hooks for Git.

Work in progress. We are using this
already to send IRC notifications and
forward updates via websockets to be able to
easily subscribe to event streams.

## Usage

### Hook

Symlink `git-post-receive-zre` to the target bare Git repository
as `hooks/post-receive`.

### Clients

#### `cat`

Pretty print incoming commits and tags similar to `git log`

```
git-post-receive-cat --include-refs 'heads/master'
```

#### `exec`

Execute command on received batch passing last commit revision as `GIT_REV`
environmental variable:

```
git-post-receive-exec --branch master --command 'echo "$GIT_BRANCH $GIT_REV"'
```

or execute command for each received commit:

```
git-post-receive-exec --branch master --per-commit --command 'echo "$GIT_BRANCH $GIT_REV"'
```

#### `zre2irc`

Send IRC notification using [ircbridge](https://github.com/sorki/ircbridge).
