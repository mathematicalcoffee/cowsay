# Cowsay

This is an R version of cowsay.

Update: I just noticed that independently, @sckott has also done a cowsay for R here: https://github.com/sckott/cowsay.
I will continue with mine anyway, it's instructive (and fun).

Notes for me: trying to follow this model: http://nvie.com/posts/a-successful-git-branching-model/

Meaning:

* branch 'master' is always production ready. We **only** merge it when it's production-ready.
* branch 'develop' for developing, if you merge with master it means that's a release
* feature branches must branch from 'develop' and must merge back into 'develop' (`merge --no-ff myfeature`; delete once done)
* release branches must branch from 'develop' and must merge back into 'develop' and 'master'. Named `release-*`.
  "The key moment to branch off a new release branch from `develop` is when develop (almost) reflects the desired state of the new release". Then you do stuff like bump version numbers etc on the release branch. When ready, merge the release branch into master and tag. Then also merge this branch into develop, and DELETE the release branch.
* hotfix branches are `hotfix-*`. Branch off from `master`, merge into `develop` and `master`. Unplanned fixes to a live production version.
