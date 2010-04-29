#!/usr/bin/env ikarus --r6rs-script

(import (ikarus) (objc) (Cocoa) (Cocoa helpers))

(define-class NSTableView)
(define-class NSTableColumn)
(define-class NSColor)
(define-class NSButton)
(define-class NSImageView)

(define pool [$ [$ NSAutoreleasePool alloc] init])

(make-app)
[$ NSApplication sharedApplication]

(define (setup-menu app-name)
  (define (make-menu title)
    [$ [$ NSMenu alloc] initWithTitle: (nsstring title)])
  [$ NSApp setMainMenu: (make-menu "")]
  (let ([apple-menu (make-menu "")])
    (define (add-item name key action mod)
      (let ([x [$ apple-menu 
                  addItemWithTitle: (nsstring name)
                  action: (get-selector action)
                  keyEquivalent: (nsstring key)]])
        (when mod
          [$ x setKeyEquivalentModifierMask: mod])))
    (define (add-separator)
      [$ apple-menu addItem: [$ NSMenuItem separatorItem]])
    (add-item (string-append "About " app-name) ""
              "orderFrontStandardAboutPanel:" #f)
    (add-separator)
    (add-item (string-append "Hide " app-name) "h" "hide:" #f)
    (add-item "Hide Others" "h" "hideOtherApplications:"
              (bitwise-ior NSAlternateKeyMask NSCommandKeyMask))
    (add-item "Show All" "" "unhideAllApplications:" #f)
    (add-separator)
    (add-item (string-append "Quit " app-name) "q" "terminate:" #f)
    (let ([menu-item 
           [$ [$ NSMenuItem alloc]
              initWithTitle: (nsstring "")
              action: #f
              keyEquivalent: (nsstring "")]])
      [$ menu-item setSubmenu: apple-menu]
      [$ [$ NSApp mainMenu] addItem: menu-item]
      [$ NSApp setAppleMenu: apple-menu]
      [$ menu-item release])
    [$ apple-menu release]))

(setup-menu "Hello Ikarus")

(define style
  (bitwise-ior 
    NSTitledWindowMask
    NSClosableWindowMask 
    NSResizableWindowMask
    NSMiniaturizableWindowMask))

(define backing NSBackingStoreBuffered)

(define win 
  [$ [$ NSWindow alloc]
     initWithContentRect: '#(#(400 500) #(400 500))
     styleMask: style
     backing: backing
     defer: #f])

[$ win setTitle: (nsstring "Hello Ikarus")]
;[$ win setAlphaValue: 3/4] ; cute

#; ; button test
(let ([btn [$ [$ NSButton alloc] init]])
  [$ btn setTitle: (nsstring "Quit")]
  [$ btn setTarget: NSApp]
  [$ btn setAction: (get-selector "terminate:")]
  [$ win setContentView: btn])

#;
(let ([table [$ [$ NSTableView alloc] init]])
  [$ table setBackgroundColor: [$ NSColor blueColor]]
  (printf "~s\n" [$ table headerView])

  (let ([col [$ [$ NSTableColumn alloc] init]])
    [$ table addTableColumn: col]
    [$ [$ col headerCell] setStringValue: (nsstring "header")])
  [$ win setContentView: table]
  [$ win setDelegate: table])

[$ win makeKeyAndOrderFront: win]

[$ NSApp run]
[$ pool release]

(printf "back!\n")
