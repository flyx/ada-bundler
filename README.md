# ada-bundler

**ada-bundler** transparently wraps the mechanisms for accessing resource files (e.g. configuration or data files) on different operating systems. It consists of a library
that provides the application with an interface to access files, and a tool which
automates the bundling process of the application.

## Current Status

 * MacOSX: In progress
 * Windows: In progress
 * Linux_Universal: In progress

## Library

The library provides an API to access files of your application. As you can access the
files with numerious IO APIs, the library will return a string containing an absolute
path to the file you requested.

## Tool

The command-line tool provided by ada-bundler takes care of the post-compilation
process of your application. You tell it the directories where the resource files
are located, and it copies them into an output folder along with your executable(s).
Third-party dynamically linked libraries will also be included in the bundle.

The tool is implemented in **Python**, for various reasons. Firstly, it is a single
file which is very portable. Secondly, Python has better support for loading configuration
files and doing file system operations. The tool takes two optional arguments:

 * The configuration file. This file is written in YAML and defines the files the
   project consists of. Defaults to `bundle.yaml`.
 * The target system name. This defaults to the current operating system and can
   currently take the values `osx`, `windows` and `linux`.

## Backends

 * __MacOSX__: ada-bundler will create an OSX App bundle for your application. You can
   copy this bundle everywhere and just double-click it to start your
   application.
           
   User data files will be stored to and loaded from the 
   `~/Library` folder.
 * __Windows__: ada-bundler will move the executable directly into a directory bearing
   the name of your application, along with your resource files. As there is no standard
   layout of this folder, ada-bundler defines his own standard. Note that ada-bundler
   cannot stuff the resource files into your executable, as most Windows applications
   do. They wouldn't have a path there, which is required by the platform-agnistic
   interface.
            
   User data files will be located in places as defined by the 
   [CSIDL (constant special item ID list)][1].
 * __Linux_Universal__: Similar to _Windows_, but locates user-specific data folders
   according to the [XDG Base Directory Specification][2].

In the future, backends for RPM and DEB packages may be added.

## Contact

Use the issue tracker of the [GitHub project][3].

## License

ada-bundler is available under the [GNAT Modified GPL][4]


 [1]: http://msdn.microsoft.com/en-us/library/bb762494.aspx
 [2]: http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html
 [3]: https://github.com/flyx86/ada-bundler
 [4]: http://en.wikipedia.org/wiki/GNAT_Modified_General_Public_License