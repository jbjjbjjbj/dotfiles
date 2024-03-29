self: super: {
    defaultPythonEnv = super.buildEnv {
        name = "defaultPythonEnv";
        paths = [
            (self.python39.withPackages (
                ps: with ps; [
                    ipython pep8
                    python-lsp-server

                    # Math stuff
                    numpy scipy matplotlib

                ]
                ))
            ];
        };
    }
