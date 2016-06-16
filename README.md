# jawa2java: Translate Jawa to Java.


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
