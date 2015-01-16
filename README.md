# Cowsay

This is an R version of cowsay.

Update: I just noticed that independently, @sckott has also done a cowsay for R here: https://github.com/sckott/cowsay.
I will continue with mine anyway, it's instructive (and fun).

## Usage

Use the `cowsay()` function with the text you want your cow to say. By default this uses the 'default' cow.
There is also `randomcowsay` which gives you a random cow and random style.

```{r}
library(cowsay)
cowsay('moo!')

#  ______
# < moo! >
#  ------
#         \   ^__^
#          \  (oo)\\_______
#             (__)\\       )\\/\\
#                 ||----w |
#                 ||     ||

library(fortunes)
cowsay(fortune())

#  _____________________________________________________________
# / Dirk Eddelbuettel: Devel is in the detail.<x>Douglas Bates: \
# | Typo or profound insight?                                   |
# | Dirk Eddelbuettel and Douglas Bates                         |
# | Rcpp-devel                                                  |
# \ February 2012                                               /
#  -------------------------------------------------------------
#         \   ^__^
#          \  (oo)\\_______
#             (__)\\       )\\/\\
#                 ||----w |
#                 ||     ||
#
```

You can specify a number of characteristics of the cow (like the original cowsay):

* which cow to use (see `list.cows()`). By default we have all the cows from the original cowsay (`cowsay(..., cow='three-eyes')`)
* the cow's eyes (`cowsay(..., eyes='Oo')`)
* the cow's tongue (`cowsay(..., tongue='U')`)
* the length at which the message is wrapped (`cowsay(..., wrap=60)`); -1 for no wrapping
* whether the cow is speaking or thinking (`cowsay(..., think=TRUE)`)
* the style of cow: this will override the `eyes` and `tongue` that you set (`cowsay(..., style='stoned')`). See `cow.styles` for the list of cow styles.


## Types of cow

Use `list.cows()` to see your available cows.

* beavis.zen
* bong
* bud-frogs
* bunny
* cheese
* cower
* daemon
* default
* dragon-and-cow
* dragon
* elephant-in-snake
* elephant
* eyes
* flaming-sheep
* ghostbusters
* head-in
* hellokitty
* kiss
* kitty
* koala
* kosh
* luke-koala
* mech-and-cow
* meow
* milk
* moofasa
* moose
* mutilated
* ren
* satanic
* sheep
* skeleton
* small
* sodomized
* stegosaurus
* stimpy
* supermilker
* surgery
* telebears
* three-eyes
* turkey
* turtle
* tux
* udder
* vader-koala
* vader
* www

## Cow styles

See `cow.styles` to see the available cow styles.

* borg: the cow looks like a Borg
* dead: the cow is dead (!)
* default: the default plain cow
* greedy: the cow is greedy
* paranoid: the cow is paranoid
* stoned: the cow is stoned
* tired: the cow is sleepy
* wired: the cow has had too much caffeine
* young: the cow in its younger days

## Add your own cows

Cows can be either R cows, Perl cows (like the original cowsay), or plain-text cows.
It's preferred that you add either an R cow or plain cow; Perl cows only work properly if the user has Perl installed on their system.

Cows are just a plain-text file with extension '.cow' or '.rcow', possibly with an additional R script.
To tell the `cowsay` package where the cows are, set your `COWPATH` environment variable to the directory these cows are in. You can put multiple paths in here, separated by ';' (Windows) or ':' (Linux), e.g.

```
export COWPATH=$HOME/.cows:/usr/share/cowsay/cows
```

### Plain-text cows

These are the simplest sorts of cows; they are just a plaintext file with the ASCII that is the cow in it. It has extension '.cow'.

Use `$eyes`, `$tongue` and `$thoughts` in the file. `$eyes` and `$tongue` are replaced by the user-specified eyes and tongue, being strings of length two (if the user specifies longer eyes only the first two characters are used; if they specify a one-character eye it is padded with a space). The `$thoughts` token is the 'stem' part of the speech bubble, not the speech bubble itself. For a cow that is thinking, this is 'o' (dots leading up to the thought bubble); for a cow that is speaking, this is '\\' (speech bubble stem).

**Any** line starting with a '#' will be ignored, **even if it is part of your cow!**. If your cow has lines that start with '#' that should be part of the cow, indent the entire cow one space.

Example (the default cow, you can see him at the start of the Readme):

```
# e.g. as default.cow
        $thoughts   ^__^
         $thoughts  ($eyes)\\_______
            (__)\\       )\\/\\
             $tongue ||----w |
                ||     ||
```

### R cows

An R cow is a plaintext cow (extension '.rcow') with an extra R file.
The R file must have extension '.r' or '.R' and have the same name as its associated cow.
For example, 'default.rcow' with 'default.r'.

Any code in the R file is executed, **then** the rcow file is read as for a plain-text cow.
You typically use the R file to modify the eyes, thoughts or tongue before it is fed in to the cowfile.
In the R file, you have the variables `eyes`, `thoughts` and `tongue` (the user-specified values) that you can modify. These modified values are used in the cowfile.

For example, if `default.r` had:

```{r}
# convert the eyes to lowercase
eyes <- tolower(eyes)
```

And `default.rcow` just had the default cow, then the eyes that the user passes in will be converted to lowercase before the cow is read in.

So in summary: Rcows are plaintext cows, where you can do preprocessing of the eyes/tongue/thoughts in an R file prior to reading in the cow.


### Perl cows

A Perl cow has extension '.cow', and it's got the same format as the cows in the original cowsay (which was written in Perl).

That is, it's just a perl script that:

* assigns some value to `$the_cow`, which is the resulting cow
* has access to the variables `$eyes`, `$tongue` and `$thoughts`
* can have Perl code in it (but only `$the_cow` is used for the resulting cow).

Note that if the user does not have Perl installed on their system, the cow will display, but probably with all of the Perl code in it too (we try to do some rudimentary preprocessing like substituting in the `$eyes`, `$tongue` and `$thoughts` tokens, and extracting just the text between `$the_cow =<<"EOC";` and the final `EOC` as the cow, but it's a poor substitute for actually having Perl).

## Devnotes

Notes for me: trying to follow this model: http://nvie.com/posts/a-successful-git-branching-model/

Meaning:

* branch 'master' is always production ready. We **only** merge it when it's production-ready.
* branch 'develop' for developing, if you merge with master it means that's a release
* feature branches must branch from 'develop' and must merge back into 'develop' (`merge --no-ff myfeature`; delete once done)
* release branches must branch from 'develop' and must merge back into 'develop' and 'master'. Named `release-*`.
  "The key moment to branch off a new release branch from `develop` is when develop (almost) reflects the desired state of the new release". Then you do stuff like bump version numbers etc on the release branch. When ready, merge the release branch into master and tag. Then also merge this branch into develop, and DELETE the release branch.
* hotfix branches are `hotfix-*`. Branch off from `master`, merge into `develop` and `master`. Unplanned fixes to a live production version.
