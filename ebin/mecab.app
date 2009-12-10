{application, mecab, [{description, "MeCab"},
                      {vsn, "1.0.0"},
                      {modules, [mecab, mecab.app, mecab.sup, mecab.server]},
                      {registered, [mecab.sup, mecab.server]},
                      {applications, [kernel, stdlib]},
                      {mod, {mecab.app, []}}]}.
