parentDir = NotebookDirectory[];
While[True,
  parentDir = ParentDirectory[parentDir];
  initFile = parentDir <> "/packages/init.m";
  If[FileExistsQ[initFile],
    Get[initFile];
    Break[];
  ];
  If[StringMatchQ[parentDir, "/"],
    Throw["Did not find packages directory"];
    Break[];
  ];
];
