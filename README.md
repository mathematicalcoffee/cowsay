# Cowsay

This is an R version of cowsay.

Update: I just noticed that independently, @sckott has also done a cowsay for R here: https://github.com/sckott/cowsay. I will continue with mine anyway, it's instructive (and fun).

## Usage

Use the `cowsay()` function with the text you want your cow to say. By default this uses the 'default' cow. There is also `randomcowsay` which gives you a random cow and random style.

```r
library(cowsay)
cowsay('moo!')

#  ______
# < moo! >
#  ------
#         \   ^__^
#          \  (oo)\_______
#             (__)\       )\/\
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
#          \  (oo)\_______
#             (__)\       )\/\
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

Use `list.cows()` to see your available cows, in case I haven't kept this up-to-date.

* apt
* beavis.zen
* bong
* bud-frogs
* bunny
* calvin
* cheese
* cock (a chicken. Get your mind out of the gutter! This is what is called in my Ubuntu 14.04 repository version of cowsay...)
* cower
* daemon
* default
* dragon-and-cow
* dragon
* duck
* elephant-in-snake
* elephant
* eyes
* flaming-sheep
* ghostbusters
* gnu
* head-in*
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
* pony
* pony-smaller
* ren
* satanic
* sheep
* skeleton
* small
* snowman
* sodomized*
* sodomized-sheep*
* stegosaurus
* stimpy
* supermilker
* surgery
* suse
* three-eyes
* turkey
* turtle
* tux
* udder
* unipony
* unipony-smaller
* vader-koala
* vader
* www

The cows marked '*' are considered "rude" so do not appear in `list.cows()` or `randomcowsay()` unless you set `rude=TRUE`. See also [Rude cows](#rude-cows).

### Rude cows

The asterisked cows in the list above are considered "rude". By default, `list.cows()` will not show them and `randomcowsay()` will never give you a rude cow. If you wish to include rude cows, you may use `rude=TRUE` in the function arguments, e.g.  `list.cows(rude=TRUE)`. You can set this option permanently by calling `cowsayOptions('rude', TRUE)`, and then you don't have to set the `rude` argument explicitly in each call. Likewise, `cowsayOptions('rude', FALSE)` will turn off rude cows (the default).

To see which cows are considered rude, use `cowsayOptions('rude.cows')`. This is a vector of the names of the rude cows (not including extension). To modify the list of rude cows, use `cowsayOptions('rude.cows', new.rude.cows)`. The default rude cows are 'sodomized', 'sodomized-sheep' and 'head-in' (I didn't make these up myself! They were included in my install of cowsay so being faithful to the original I included them!). 

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

Cows can be either R cows, Perl cows (like the original cowsay), or plain-text cows. It's preferred that you add either an R cow or plain cow; Perl cows only work properly if the user has Perl installed on their system. Cows are just a plain-text file with extension '.cow' or '.rcow', possibly with an additional R script. Once you create your cow, you can provide the path to the cowfile in the `cow` argument of `cowsay`, or you can add the path to `cowsay`'s search list (see [Adding to the cow search path](#adding-to-the-cow-search-path)).

### Plain-text cows

These are the simplest sorts of cows; they are just a plaintext file with the ASCII that is the cow in it. It has extension '.cow'.

Use `$eyes`, `$tongue` and `$thoughts` in the file. `$eyes` and `$tongue` are replaced by the user-specified eyes and tongue, being strings of length two (if the user specifies longer eyes only the first two characters are used; if they specify a one-character eye it is padded with a space). The `$thoughts` token is the 'stem' part of the speech bubble, not the speech bubble itself. For a cow that is thinking, this is 'o' (dots leading up to the thought bubble); for a cow that is speaking, this is '\\' (speech bubble stem).

**Any** line starting with a '#' will be ignored, **even if it is part of your cow!**. If your cow has lines that start with '#' that should be part of the cow, indent the entire cow one space.

Example (the default cow, you can see him at the start of the Readme):

```
# e.g. as default.cow
        $thoughts   ^__^
         $thoughts  ($eyes)\_______
            (__)\       )\/\
             $tongue ||----w |
                ||     ||
```

### R cows

An R cow is a plaintext cow (extension '.rcow') with an extra R file. The R file must have extension '.r' or '.R' and have the same name as its associated cow. For example, 'default.rcow' with 'default.r'.

Any code in the R file is executed, **then** the rcow file is read as for a plain-text cow. You typically use the R file to modify the eyes, thoughts or tongue before it is fed in to the cowfile. In the R file, you have the variables `eyes`, `thoughts` and `tongue` (the user-specified values) that you can modify. These modified values are used in the cowfile. For example, if `default.r` had:

```{r}
# convert the eyes to lowercase
eyes <- tolower(eyes)
```

And `default.rcow` just had the default cow, then the eyes that the user passes in will be converted to lowercase before the cow is read in.

So in summary: Rcows are plaintext cows, where you can do preprocessing of the eyes/tongue/thoughts in an R file prior to reading in the cow.

**SECURITY NOTE**: of course **any** code in the '.r' file is executed, so someone could put malicious code in here and send the file to you under the guise of it being a cowfile. **Check any R files that claim to be cows before you use them!** I am not responsible if someone deletes all your files in their Rcow (which then shows you the satanic-style cow saying "MUAHAHAHA YOU IDIOT"). In terms of what is distributed with this package, the only two cows with R files are [three-eyes](https://github.com/mathematicalcoffee/cowsay/blob/master/inst/cows/three-eyes.r), which makes a three-eyed cow (converts the two-character eyes to three characters) and [udder](https://github.com/mathematicalcoffee/cowsay/blob/master/inst/cows/udder.r), which requires the eyes to have a space in between them. You may verify for yourself that this is all the R files do.


### Perl cows

A Perl cow has extension '.cow', and it's got the same format as the cows in the original cowsay (which was written in Perl).

That is, it's just a perl script that:

* assigns some value to `$the_cow`, which is the resulting cow
* has access to the variables `$eyes`, `$tongue` and `$thoughts`
* can have Perl code in it (but only `$the_cow` is used for the resulting cow).

Note that if the user does not have Perl installed on their system, the cow will display, but probably with all of the Perl code in it too (we try to do some rudimentary preprocessing like substituting in the `$eyes`, `$tongue` and `$thoughts` tokens, and extracting just the text between `$the_cow =<<"EOC";` and the final `EOC` as the cow, but it's a poor substitute for actually having Perl).

### Adding to the cow search path

To tell the `cowsay` package where the cows are, set your `COWPATH` environment variable to the directory these cows are in. You can put multiple paths in here, separated by ';' (Windows) or ':' (Linux), e.g.

```
export COWPATH=$HOME/.cows:/usr/share/cowsay/cows
```

The `cowsay` package always adds the path to the cows provided in this package on the end, i.e. `system.file('cows', package='cowsay')`. Cows found earlier on the `COWPATH` override those found later.

## Devnotes

Notes for me: trying to follow this model: http://nvie.com/posts/a-successful-git-branching-model/

Meaning:

* branch 'master' is always production ready. We **only** merge it when it's production-ready.
* branch 'develop' for developing, if you merge with master it means that's a release
* feature branches must branch from 'develop' and must merge back into 'develop' **and also perl-cows** (to keep it in sync while preserving the perl cow support) (`merge --no-ff myfeature`; delete once done)
* release branches must branch from 'develop' and must merge back into 'develop' and 'master'. Named `release-*`.
  "The key moment to branch off a new release branch from `develop` is when develop (almost) reflects the desired state of the new release". Then you do stuff like bump version numbers etc on the release branch. When ready, merge the release branch into master and tag. Then also merge this branch into develop, and DELETE the release branch.
* hotfix branches are `hotfix-*`. Branch off from `master`, merge into `develop` and `master`. Unplanned fixes to a live production version.

Deleting a branch: we tag them first so if we want, we can go back to the point that the branches were. Following [this stackoverflow answer](http://stackoverflow.com/questions/10242924/how-to-close-a-branch-without-removing-it-from-history-in-git):

```
git checkout <branchname>
git tag archive/<branchname> <branchname>
git checkout develop
# merge
git merge <branchname>
# delete
git branch -d <branchname>
git push origin :<branchname>
git push --tags
```

So that if we wish to go back to the last commit of that branch we can find the tag `archive/<branchname>`. Unsure if this is useful, but I'll try it for a bit and see.
