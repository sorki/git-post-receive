hsuper: hself: {
  git-post-receive-types =
    hself.callCabal2nix "git-post-receive-types" ./git-post-receive-types {};
  git-post-receive-hook =
    hself.callCabal2nix "git-post-receive-hook" ./git-post-receive-hook {};
  git-post-receive-cat =
    hself.callCabal2nix "git-post-receive-cat" ./git-post-receive-cat {};
  git-post-receive-exec =
    hself.callCabal2nix "git-post-receive-exec" ./git-post-receive-exec {};
  git-post-receive-push =
    hself.callCabal2nix "git-post-receive-push" ./git-post-receive-push {};
  git-post-receive-zre =
    hself.callCabal2nix "git-post-receive-zre" ./git-post-receive-zre {};
  git-post-receive-zre2irc =
    hself.callCabal2nix "git-post-receive-zre2irc" ./git-post-receive-zre2irc {};
}