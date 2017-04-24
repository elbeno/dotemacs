# .emacs configuration

## Quick minimalist setup for building org-mode presentations

```bash
$ git clone git@github.com:elbeno/dotemacs
$ cd dotemacs
$ git checkout minimalist-org-reveal
$ ln -s ./.emacs ~/.emacs
```

Then when you run emacs, `C-c C-e R R` will output an org-mode presentation to the ox-reveal back end.
