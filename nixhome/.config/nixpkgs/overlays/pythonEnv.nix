self: super: {
    defaultPythonEnv = super.buildEnv {
        name = "defaultPythonEnv";
        paths = [
            (self.python3.withPackages (
                ps: with ps; [
                    ipython python-language-server pep8
                    jupyterlab jupyter-c-kernel

                    # Math stuff
                    numpy scipy matplotlib
                ]
                ))
            ];
        };
    }
