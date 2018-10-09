# Intro to `git` + Gitlab.

This is a tutorial on how to do basic things with `git`, that are neccesary for working on a
multi-developer project with multiple private repositories hosted on gitlab.com

## Installing git

Mileage may vary depending on the OS:

* [Windows](https://gitforwindows.org)

* On OSX:  `brew install git`

* On Ubuntu: `sudo apt-get install git`

* [More official doc](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git) on installing
  `git` on various operating systems:

## Configure git

One of the first things to do upon clean install of git is to configure the user. Email shoul dmatch
the one on gitlab, then all of the commits will be match up together the gitlab user:

```
$ git config --global user.email my-email@example.com
$ git config --global user.name "FistName LastName"
```

Next one is a very useful global list og ignore patterns:

```
$ git config --global core.excludesfile $HOME/.gitignore_global
```

And check current settings (instead of above commands it's possible to simply modify the file):

```
$ cat ~/.gitconfig
[user]
        name = FirstName LastName
        email = my-emai@example.com
[core]
        excludesfile = /home/username/.gitignore_global
```

Adding ignore patterns are very much specific to personal use, editors utilized and types of project
git is used for. Here is a suggested list for Haskell+emacs development environment:

```
.dir-locals.el
*~
dist
.cabal-sandbox
cabal.sandbox.config
.stack-work*
.ghci
.hindent.yaml
*.o
*.p_o
*.hi
*.chi
*.chs.h
*.dyn_o
*.dyn_hi
```

_Side note_ - It is possible to add files that are being ignored with `git add -f` command.

## Authentication

A private repository is created on gitlab.com and that's where it lives, but in order to start
working on it we need a local copy. The process of getting it locally is called cloning. Becuase it
is a private repo that we are talking about, we'll need to authenticate ourselves in one of the
three available methods:

* (bad) Type in a user name and password at the command line each time we want to access the remote
  repo. This is a very unproductive way to do work, moreover it's impossible with MFA turned on. So
  we just forget about it.

* (bad) Create a personal Access Token and use it inside the repo URL for authentication. I am
  actually not 100% positive it can be used for development, since I only see `read_repository`
  permission in gitlab, which means you can clone and pull changes, but not make any updates to the
  repository. So we'll won't talk about this method to much, but it will be relevant a bit later.

* (good) The proper way to do development is to use a personal SSH key. This is what I'll explain
  next.

Which of the above methods is used for authentication is decided from the url that was used for
cloning a repository:

```shell
$ git clone git@gitlab.com:user-or-group/repo-name.git
```

Above command will use `ssh` to communicate with gitlab and make a local copy of the repo into the
`./repo-name` folder. But it won't work just yet, we need to setup authentication with ssh.

If instead you had used `https://gitlab.com/user-or-group/repo-name.git` or
`https://username:access_token@gitlab.com/user-or-group/repo-name.git` then the earlier mentioned
methods would have been used. At any time a url of a cloned a repository can be changed, no need to
remove it and clone from scratch:

```shell
$ git remote get-url origin
https://gitlab.com/group/repo-name.git
$ git remote set-url origin git@gitlab.com:group/repo-name.git
```

__NOTE__ Dependencies might need to be installed: `ssh`

In order to use ssh method for authentication we need to do three things:

* [Generate a private/public keypair](https://docs.gitlab.com/ee/ssh/#generating-a-new-ssh-key-pair) (or use an existing one if such is available):

```shell
$ ssh-keygen -o -t rsa -C "your.email@example.com" -b 4096
```

* [Add ssh key to `ssh-agent`](https://docs.gitlab.com/ee/ssh/#working-with-non-default-ssh-key-pair-paths). Important is to make sure agent is running. Also adding the key will prevent it from asking for a password every time.

```shell
$ eval $(ssh-agent -s)
$ ssh-add ~/.ssh/id_rsa
```

* [Upload contents of a public key](https://docs.gitlab.com/ee/ssh/#adding-a-ssh-key-to-your-gitlab-account) (if default name was used it's gonna be `~/.ssh/id_rsa.pub`) to
  Gitlab at https://gitlab.com/profile/keys


At this point we should be able to access our private repo with git.

```shell
$ git clone git@gitlab.com:user-or-group/repo-name.git
$ cd repo-name
```

## Repository structure

### Commit

A commit is one of the most important concepts in git. It records the changes performed together
with a message, authors, and the `sha` for the parent commits in the repository. In it's core git repo
is just a Merkel tree with data attached to it. Here is how we can look at the latest commit (more later on `HEAD`)

```shell
$ git show HEAD
commit 02367ff9f7ccd2c6d6e15148c8039f2fb5932b97 (HEAD -> master, origin/master, origin/HEAD)
Author: Alexey Kuleshevich <lehins@yandex.ru>
Date:   Tue Oct 9 14:54:45 2018 +0000

    Initial commit

diff --git a/README.md b/README.md
new file mode 100644
...
```

If you imagine a tree with commits being nodes and edges being pointers to previous commits in
history, with initial commit being at the root of it, then you should be able to picture the whole
repository as a graph of changes to files and directories in that repo.

### Branches

A branch is just a way to keep track of an alternate history, it is simply a way to point to a
particular commit in that graph of commits. In reality, a branch is nothing less than just a file
that has a `sha` of a commit in it. Nevertheless, branches are crucial in deveopment.

In each repository there is always a main branch, usually named `master`. Most of the time commits
should NOT be added directly to the `master` branch and all changes should be done to feature
branches instead, which later get merged into `master` through gitlab UI with a help of Merge
Request (MR) process.

There is distinction between local branches and remote ones. The way to disambiguate an `origin/`
prefix is used. For example, the commit your local `master` branch is pointing at, can be totally
different from what `origin/master`.

### HEAD

At any point in time your local repository must point to a particular place in time, which is
tracked by a speacial name `HEAD`. Normally it is pointing at some branch, but it's possible to be
in a "detached `HEAD`" state, meaning it's pointing at some commit and any changes will get lost
unless new branch gets created.

## Workflow

### New branch

When we start working on an issue we almost always start by creating a branch.

We can create a branch in a couple of ways, but we always need to start a branch from some point in
the repo, either a commit `sha` or a name of a branch or a tag. In common terms, something that can
be resolved to a `sha`.

So one way to create a branch is to create it from current state of remote `master`:

* Make sure we get all of the updates from the remote state to our local one:

```
$ git fetch
```

Above command will not change any of our local branches.

* Create a branch named `foo`:

```
$ git branch foo origin/master
Branch foo set up to track remote branch master from origin.
$ git checkout foo
Switched to branch 'foo'
Your branch is up-to-date with 'origin/master'.
```

An alternative way to achieve the same thing is to checkout the branch/tag/commit you'd like to use
as the base. For example I like to keep my local `master` up to date, and most of the time I used
`master` as the base for a new branch, so this saves me time:

```
$ git checkout master
Switched to branch 'master'
Your branch is up-to-date with 'origin/master'.
$ git pull
Already up-to-date.
```

`git pull` in the above command is the combination of `git fetch` + `git merge`, but it's better not
to use that combination unless you are sure that you have not made any changes the local branch,
which is always the case for my `master`.

Now we delete the previously created branch with the same name.

```
$ git branch -d foo
Deleted branch foo (was 292dc6e).
$ git checkout -b foo
Switched to a new branch 'foo'
```

`checkout -b` will create a branch and check it out all in one command.

Currently our branch exists only locally. In order to have a matching branch on a remote repository
we'll run:

### Introduce changes

Create a file add it to the repo and push them to the remote:

```shell
$ echo "Blah Blah" > blah.txt
$ git add blah.txt
$ git commit -am "My first useless commit"
$ git push
```

Making more changes to files:
```shell
$ rm blah.txt
$ echo "# Booyah" > booyah.md
$ git status
$ git add -A # will add all files
$ git commit -a # Will open you editor
$ git push
```

Once done creating Merge Request in th UI is easy.

### Working with modules

`git` submodules are somewhat complicated beasts, but we don't need to go into much detail in
order to be able to use them in a read only fashion.

A quick motivation why they might be useful. In a common case when one private repository depends on
another private repository we need to be able to automate that dependency not only locally, but also
on CI. Problem with CI is that it is only possible to clone another private repo with a help of a
secret token over HTTPS described earlier and the only way to deal with that issue is to use
relative urls, which git submodules have native support for.

So, here is what we need to do locally in order to clone a repository that contains submodules:

```shell
$ git clone --recursive git@gitlab.com:user-or-group/repo-name.git
```

If a repo has been already cloned, then we need to run `--init`:
```shell
$ git submodule update --init --recursive
```

Afterwards, whenever a submodule been updated, we just run:

```
$ git submodule update --recursive --remote
```


