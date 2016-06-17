# jawa2java: Translate Jawa to Java.
[![License](https://img.shields.io/badge/License-EPL%201.0-red.svg)](https://opensource.org/licenses/EPL-1.0)
[![Download](https://api.bintray.com/packages/arguslab/maven/jawa2java/images/download.svg)](https://bintray.com/arguslab/maven/jawa2java/_latestVersion)
[![Build Status](https://travis-ci.org/arguslab/jawa2java.svg?branch=master)](https://travis-ci.org/arguslab/jawa2java)
[![Codacy Badge](https://api.codacy.com/project/badge/Grade/0889e55e2bc04573a3b12a9fa1dd5ce7)](https://www.codacy.com/app/fgwei521/jawa2java?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=arguslab/jawa2java&amp;utm_campaign=Badge_Grade)
[![Codacy Badge](https://api.codacy.com/project/badge/Coverage/0889e55e2bc04573a3b12a9fa1dd5ce7)](https://www.codacy.com/app/fgwei521/jawa2java?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=arguslab/jawa2java&amp;utm_campaign=Badge_Coverage)

## Developing jawa2java

In order to take part in jawa2java development, you need to:

1. Install the following software:
    - IntelliJ IDEA 14 or higher with compatible version of Scala plugin

2. Fork this repository and clone it to your computer

  ```
  $ git clone https://github.com/arguslab/jawa2java.git
  ```

3. Open IntelliJ IDEA, select `File -> New -> Project from existing sources`
(if from initial window: `Import Project`), point to
the directory where Scala plugin repository is and then import it as SBT project.

4. When importing is finished, go to jawa2java repo directory and run

  ```
  $ git checkout .idea
  ```

  in order to get artifacts and run configurations for IDEA project.
  
5. If you want to build jawa2java from command line, go to jawa2java repo directory and run

   ```
   $ tools/bin/sbt clean compile test
   ```

## How to contribute

To contribute to the jawa2java, please send us a [pull request](https://help.github.com/articles/using-pull-requests/#fork--pull) from your fork of this repository!

For more information on building and developing jawa2java, please also check out our [guidelines for contributing](CONTRIBUTING.md). People who provided excellent ideas are listed in [contributor](CONTRIBUTOR.md).
