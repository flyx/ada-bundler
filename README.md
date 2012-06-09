# ada-bundler

**ada-bundler** transparently wraps the mechanisms of accessing resource files (e.g. configuration or data files) on different operating systems. It consists of a library
that provides the application with an interface to access files, and a tool which
automates the bundling process of the application.

## Library

The library provides an API to access files of your application. As you can access the
files with numerious IO APIs, the library will return a string containing an absolute
path to the file you requested.

## Tool

The command-line tool provided by ada-bundler takes care of the post-compilation
process of your application. You tell him the directories where the resource files
are located, and it copies them into an output folder along with your executable(s).
This output directory is usually called `bundle`.

## Backends

 * MacOSX: ada-bundler will create an OSX App bundle for your application. You can
           copy this bundle everywhere and just double-click it to start your
           application. You can provide a list of binary files (e.g. third-party
           dependencies) which will be bundled along with your files into the
           App bundle.
           
           User data files will be stored to and loaded from
           `~/Library/Application Support/[application-name]`.
 * Windows: ada-bundler will move the executable directly into the `bundle` directory
            and your resource files into subdirectories. As there is no standard
            layout of this folder, ada-bundler defines his own standard.
            
            User data files will be located in places as defined by the 
            [CSIDL (constant special item ID list)][1]
 * Linux: ada-bundler will take the `bundle` folder as root directory and copy your
          files into it according to the [XDG Base Directory Specification][2]

## Contact

Use the issue tracker of the [GitHub project][3].

## License

ada-bundler is available under the [GNAT Modified GPL][4]


 [1]: http://msdn.microsoft.com/en-us/library/bb762494.aspx
 [2]: http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html
 [3]: https://github.com/flyx86/ada-bundler
 [4]: http://en.wikipedia.org/wiki/GNAT_Modified_General_Public_License