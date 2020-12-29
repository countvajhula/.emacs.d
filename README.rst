.emacs.d
========
My Emacs configuration.

If you are interested in using this as your Emacs config, just clone the repo to :code:`~/.emacs.d` **but make sure** to use the :code:`public` rather than the :code:`master` branch. And then launch Emacs. Something like:

.. code-block:: bash

  cd ~
  mv .emacs.d my.emacs.d  # backup your original .emacs.d, if you need it
  git clone git@github.com:countvajhula/.emacs.d.git
  cd .emacs.d
  git branch -t public origin/public
  git checkout public

This repo uses `straight.el <https://github.com/raxod502/straight.el>`_ for package management, and the :code:`public` branch relies exclusively on publicly accessible package repositories, so it should work out of the box in your environment. On the other hand, the :code:`master` branch may use local development versions of package repositories rather than publicly accessible ones and may fail to find those in your environment.
