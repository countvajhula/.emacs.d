.emacs.d
========
An `evil <https://www.emacswiki.org/emacs/Evil>`__-based Emacs configuration that uses `Rigpa <https://github.com/countvajhula/rigpa>`__ as its dominant paradigm.

This configuration is set up to work out of the box in any Emacs installation as it follows the approach described in `Turn Your .emacs.d Into an Emacs Distribution <https://countvajhula.com/2020/12/27/turn-your-emacs-d-into-an-emacs-distribution-with-straight-el/>`__.

If you are interested in using this as your Emacs config, just clone the repo to :code:`~/.emacs.d` **but make sure** to use the :code:`public` rather than the :code:`main` branch [1]_. And then launch Emacs. Something like:

.. code-block:: bash

  cd ~
  mv .emacs.d my.emacs.d  # backup your original .emacs.d, if you need it; otherwise, rm -rf .emacs.d to delete it
  git clone git@github.com:countvajhula/.emacs.d.git
  cd .emacs.d
  git branch -t public origin/public
  git checkout public

.. [1] This repo uses `Straight.el <https://github.com/radian-software/straight.el>`_ for package management, and the :code:`public` branch relies exclusively on publicly accessible package repositories with frozen versions, so it should work out of the box in your environment. On the other hand, the :code:`main` branch may use local development versions of package repositories rather than publicly accessible ones and may fail to find those in your environment.

"License":
==========
This work is "part of the world." You are free to do whatever you like with it and it isn't owned by anybody, not even the creators. Attribution would be appreciated and would help, but it is not strictly necessary nor required. If you'd like to learn more about this way of doing things and how it could lead to a peaceful, efficient, and creative world (and how you can be involved), visit `drym.org <https://drym.org>`_.
