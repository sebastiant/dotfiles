{ config, pkgs, ... }:
{
  programs.git = {
    enable = true;
    userName = "Sebastian Tunstig";
    userEmail = "sebastian.tunstig@gmail.com";
    aliases = {
      lg = "log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%C(bold blue)<%an>%Creset' --abbrev-commit";
      st = "status -s";
      s = "status";
      a = "!git add . && git status";
      ci = "commit";
      co = "checkout";
      d = "difftool";
      dp = "!git --no-pager diff";
      ds = "diff --staged";
      dsp = "!git --no-pager diff --staged";
      main = "checkout main";
      master = "checkout master";
      no-remotes = "!git branch --format '%(refname:short) %(upstream:short)' | awk '{if (!$2) print $1;}'";
      alias = "!git config --list | grep 'alias\\.' | sed 's/alias\\.\\([^=]*\\)=\\(.*\\)/\\1\\\t => \\2/' | sort";
    };
    extraConfig = {
      core.editor = "vim";
      init.defaultBranch = "main";
      diff.tool = "vimdiff";
      diff.colorMoved = "zebra";
      difftool.prompt = false;
      difftool.trustExitCode = true;
      merge.tool = "ediff";
      merge.keepBackup = false;
      merge.conflictStyle = "diff3";
      mergetool.keepBackup = false;
      mergetool.trustExitCode = true;
      mergetool.ediff.keepBackup = false;
      mergetool.ediff.cmd = ''
        emacs --eval \"\
        (progn\
          (defun ediff-write-merge-buffer ()\
            (let ((file ediff-merge-store-file))\
              (set-buffer ediff-buffer-C)\
              (write-region (point-min) (point-max) file)\
              (message \\\"Merge buffer saved in: %s\\\" file)\
              (set-buffer-modified-p nil)\
              (sit-for 1)))\
          (setq ediff-quit-hook 'kill-emacs\
                ediff-quit-merge-hook 'ediff-write-merge-buffer)\
          (ediff-merge-files-with-ancestor \\\"$LOCAL\\\" \\\"$REMOTE\\\"\
                                           \\\"$BASE\\\" nil \\\"$MERGED\\\"))\"
      '';
      github.user = "sebastiant";
      color.ui = true;
      fetch.prune = true;
      pull.rebase = true;
      push.default = "upstream";
    };
    ignores = [
      "\#*\#"
      ".\#*"
      "*.local"
      "TAGS"
      "tags"
      "*.iml"
      "*.ipr"
      "*.iws"
      ".idea/"
      ".gradle"
      "build/"
      "repos/"
      "*.class"
      "target"
      "target/"
      ".metadata"
      ".cache"
      ".worksheet"
      "*.class"
      ".eunit"
      "deps"
      "*.o"
      "*.beam"
      "*.plt"
      "erl_crash.dump"
      "ebin"
      "rel/example_project"
      ".concrete/DEV_MODE"
      ".rebar"
      "_build/"
      "*~"
      "*.swp"
      "*.swo"
      "__MACOSX/"
      "*.DS_Store"
      ".elasticbeanstalk/*"
      "!.elasticbeanstalk/*.cfg.yml"
      "!.elasticbeanstalk/*.global.yml"
      "ist"
      "dist-*"
      "cabal-dev"
      "*.o"
      "*.hi"
      "*.chi"
      "*.chs.h"
      "*.dyn_o"
      "*.dyn_hi"
      ".hpc"
      ".hsenv"
      ".cabal-sandbox/"
      "cabal.sandbox.config"
      "*.prof"
      "*.aux"
      "*.hp"
      "*.eventlog"
      ".stack-work/"
      "cabal.project.local"
      "cabal.project.local~"
      ".HTF/"
      ".ghc.environment.*"
      "__pycache__/"
      "*.py[cod]"
      "*$py.class"
      "pyrightconfig.json"
      "*.so"
      ".Python"
      "build/"
      "develop-eggs/"
      "dist/"
      "downloads/"
      "eggs/"
      ".eggs/"
      "lib64/"
      "parts/"
      "sdist/"
      "var/"
      "wheels/"
      "share/python-wheels/"
      "*.egg-info/"
      ".installed.cfg"
      "*.egg"
      "MANIFEST"
      "*.manifest"
      "*.spec"
      "pip-log.txt"
      "pip-delete-this-directory.txt"
      "htmlcov/"
      ".tox/"
      ".nox/"
      ".coverage"
      ".coverage.*"
      ".cache"
      "nosetests.xml"
      "coverage.xml"
      "*.cover"
      "*.py,cover"
      ".hypothesis/"
      ".pytest_cache/"
      "cover/"
      "*.mo"
      "*.pot"
      "*.log"
      "local_settings.py"
      "db.sqlite3"
      "db.sqlite3-journal"
      "instance/"
      ".webassets-cache"
      ".scrapy"
      "docs/_build/"
      ".pybuilder/"
      "target/"
      ".ipynb_checkpoints"
      "profile_default/"
      "ipython_config.py"
      "__pypackages__/"
      "celerybeat-schedule"
      "celerybeat.pid"
      "*.sage.py"
      ".env"
      ".venv"
      "env/"
      "venv/"
      "ENV/"
      "env.bak/"
      "venv.bak/"
      ".spyderproject"
      ".spyproject"
      ".ropeproject"
      "/site"
      ".mypy_cache/"
      ".dmypy.json"
      "dmypy.json"
      ".pyre/"
      ".pytype/"
      "cython_debug/"
      ".env_vars"
      ".direnv/"
      ".dir-locals.el"
      ".projectile"
      "elm-stuff"
      "repl-temp-*"
      "result"
    ];
  };
}
