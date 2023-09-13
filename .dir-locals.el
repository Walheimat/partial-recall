((nil . ((wal-project-install-default-cmd . "make package-install")
         (wal-project-execute-default-cmd . ("make update-version" "make pacify"))
         (find-sibling-rules . (("test/\\([^/]+\\)-test.el" "\\1.el")
                                ("\\([^/]+\\).el" "test/\\1-test.el"))))))
