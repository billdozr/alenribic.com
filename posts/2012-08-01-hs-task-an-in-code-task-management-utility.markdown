---
title: "Hs-task: An in-code task management utility"
author: Alen Ribic
date: August 1, 2012
tags: haskell
description: Hs-task is an in-code task management utility written in Haskell. I wrote hs-task some time back and finally got around to releasing it into the wild with hopes that others find it useful.
---
[Hs-task](https://github.com/billdozr/hs-task) is an in-code task management utility written in Haskell. 
I wrote hs-task some time back and finally got around to releasing it into the wild with hopes that others find it useful. 

Many IDEs and editors contain some form of built-in task management capabilities.
Philosophy behind `hs-task` is to provide a standlone, in-code, task management utility that can be interfaced via command-line, IDEs and editors.

`hs-task` icludes the command-line utilities `task-find` and `task-crunch` (see below for usage).

For IDE/editor integration, the Eclipse sample plugin can be located [here](https://github.com/billdozr/com.alenribic.atodo).

**Note that the project is still in its infancy as it is missing some core parts such as the persistence mechanism that will store the in-code task changes.**

**Comments, issue reports, patches and feedback are more than welcome!**

Usage:
-----------------------------

task-find [OPTION...] toplevel_dir|file_path

      -R    Recurse sub-directories

      -f    Filter by file extention

      -h    This usage info

task-crunch [OPTION...] raw_task

      -o    Output format (plain | html | xml)

      -h    This usage info

**Examples:**

To find all tasks in python source files and then output them as plain text run
> task-find -R -f .py ~/Development/python/projects/ | task-crunch

Result

    @TODO(bug, mlcomp, #alen, #bob): File path iteration fails on deep recursion | added a test case | H, 6 | "/Users/alen/Development/python/billdozr_env/projects/BilldozrML/src/parser/load_data.py", (14,1), Wed Jul 11 13:23:27 SAST 2012
    ...

And for html output run
> task-find -R -f .py ~/Development/python/projects/ | task-crunch -o html > /tmp/task-sample.html

Result

    List of Todo Task's:

      Subject: File path iteration fails on deep recursion
      Action: added a test case
      Label(s): bug, mlcomp
      User(s): alen, bob
      Priority: High
      Time spent: 6
      Source file: /Users/alen/Development/python/billdozr_env/projects/BilldozrML/src/parser/load_data.py
      Line / Column: (14,1)
      File modified: Wed Jul 11 13:23:27 SAST 2012

      ...

The cool thing here is that we can use the xml supported output which then can be pulled into any Editor/IDE we can build a plugin for.

**See:** the Eclipse [plugin sample](https://github.com/billdozr/com.alenribic.atodo) I built (barely alpha). 

Limitations:
-----------------------------

 * Currently, TODO tasks are the only tasks supported.

You can locate the project on my [GitHub page](https://github.com/billdozr).