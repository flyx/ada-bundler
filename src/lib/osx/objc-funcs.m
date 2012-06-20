#import "objc-funcs.h"

const char* get_bundle_path() {
   NSString* applicationPath = [[NSBundle mainBundle] bundlePath];
   const char* cPath = [applicationPath cStringUsingEncoding:NSASCIIStringEncoding];
   [applicationPath release];
   return cPath;
}