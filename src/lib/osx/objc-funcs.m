#import "objc-funcs.h"

char* get_bundle_path() {
   NSString* applicationPath = [[NSBundle mainBundle] bundlePath];
   char* cPath = [applicationPath UTF8String];
   [applicationPath release];
   return cPath;
}