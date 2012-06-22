#import <string.h>

#import "objc-funcs.h"

char* get_bundle_path() {
   NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
   NSString* applicationPath = [[NSBundle mainBundle] bundlePath];
   const char* cPath = [applicationPath cStringUsingEncoding:NSASCIIStringEncoding];
   // copy string because original one gets released with pool
   char* cPathCopy = (char*)malloc(strlen(cPath) * sizeof(char));
   strcpy(cPathCopy, cPath);
   [applicationPath release];
   // TODO: how to release without deleting cPathCopy
   //[pool release];
   return cPathCopy;
}