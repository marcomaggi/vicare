#!/usr/bin/env ikarus --r6rs-script
;;
;; 3-D gear wheels.  This program is in the public domain.
;;
;; Brian Paul
;;
;; Conversion to GLUT by Mark J. Kilgard
;; Conversion to GtkGLExt by Naofumi Yasufuku
;; Port to Scheme/Gauche(GtkGLExt) by Shiro Kawai
;; Port to Scheme/Gauche(GLUT) by YOKOTA Hiroshi
;; Port to Ypsilon by YOKOTA Hiroshi

(import (ypsilon-compat) (rnrs) (rnrs programs) (gl) (glut))

;; These constant values are not defined in Ypsilon yet
(define pi 3.14159265358979323846)
(define GLUT_ELAPSED_TIME 700)

(define (f32vector . lst)
  (define-syntax f32set!
    (syntax-rules ()
      ((_ bv n value)
       (bytevector-ieee-single-native-set! bv (* n 4) value))))
  (let ((bv (make-bytevector (* (length lst) 4))))
    (let loop ((i 0) (lst lst))
      (cond ((null? lst) bv)
        (else
         (f32set! bv i (car lst))
         (loop (+ i 1) (cdr lst)))))))

(define (/. a b)
  (/ (inexact a) (inexact b)))

(define (c-int->c-uchar c)
  (bitwise-and c #xff))

;; Draw a gear wheel.  You'll probably want to call this function when
;; building a display list since we do a lot of trig here.
;;
;; Input:  inner_radius - radius of hole at center
;; outer_radius - radius at center of teeth
;; width - width of gear
;; teeth - number of teeth
;; tooth_depth - depth of tooth

(define (gear inner-radius outer-radius width teeth tooth-depth)
  (let ((r0 inner-radius)
    (r1 (- outer-radius (/ tooth-depth 2.0)))
    (r2 (+ outer-radius (/ tooth-depth 2.0)))
    (da (* 2.0 (/ pi teeth 4.0))))
    (glShadeModel GL_FLAT)
    (glNormal3f 0.0 0.0 1.0)

    ;; draw front face
    (glBegin GL_QUAD_STRIP)
    (do ((i 0.0 (+ i 1.0))) ((>= i (+ teeth 1.0)))
      (let ((_angle (* i 2.0 (/ pi teeth))))
    (glVertex3f (* r0 (cos _angle)) (* r0 (sin _angle)) (* width 0.5))
    (glVertex3f (* r1 (cos _angle)) (* r1 (sin _angle)) (* width 0.5))
    (when (< i teeth)
          (glVertex3f (* r0 (cos _angle)) (* r0 (sin _angle)) (* width 0.5))
          (glVertex3f (* r1 (cos (+ _angle (* 3.0 da))))
              (* r1 (sin (+ _angle (* 3.0 da))))
              (* width 0.5)))))
    (glEnd)

    ;; draw front sides of teeth
    (glBegin GL_QUADS)
    (do ((i 0.0 (+ i 1.0))) ((>= i teeth))
      (let ((_angle (* i 2.0 (/ pi teeth))))
    (glVertex3f (* r1 (cos _angle)) (* r1 (sin _angle)) (* width 0.5))
    (glVertex3f (* r2 (cos (+ _angle da)))
            (* r2 (sin (+ _angle da)))
            (* width 0.5))
    (glVertex3f (* r2 (cos (+ _angle (* 2.0 da))))
            (* r2 (sin (+ _angle (* 2.0 da))))
            (* width 0.5))
    (glVertex3f (* r1 (cos (+ _angle (* 3.0 da))))
            (* r1 (sin (+ _angle (* 3.0 da))))
            (* width 0.5))))
    (glEnd)

    (glNormal3f 0.0 0.0 -1.0)

    ;; draw back face
    (glBegin GL_QUAD_STRIP)
    (do ((i 0.0 (+ i 1.0))) ((>= i (+ teeth 1.0)))
      (let ((_angle (* i 2.0 (/ pi teeth))))
    (glVertex3f (* r1 (cos _angle)) (* r1 (sin _angle)) (* width -0.5))
    (glVertex3f (* r0 (cos _angle)) (* r0 (sin _angle)) (* width -0.5))
    (when (< i teeth)
          (glVertex3f (* r1 (cos (+ _angle (* 3.0 da))))
              (* r1 (sin (+ _angle (* 3.0 da))))
              (* width -0.5))
          (glVertex3f (* r0 (cos _angle)) (* r0 (sin _angle)) (* width -0.5)))))
    (glEnd)

    ;; draw back sides of teeth
    (glBegin GL_QUADS)
    (do ((i 0.0 (+ i 1.0))) ((>= i teeth))
      (let ((_angle (* i 2.0 (/ pi teeth))))
    (glVertex3f (* r1 (cos (+ _angle (* 3.0 da))))
            (* r1 (sin (+ _angle (* 3.0 da))))
            (* width -0.5))
    (glVertex3f (* r2 (cos (+ _angle (* 2.0 da))))
            (* r2 (sin (+ _angle (* 2.0 da))))
            (* width -0.5))
    (glVertex3f (* r2 (cos (+ _angle da)))
            (* r2 (sin (+ _angle da)))
            (* width -0.5))
    (glVertex3f (* r1 (cos _angle)) (* r1 (sin _angle)) (* width -0.5))))
    (glEnd)

    ;; draw outward faces of teeth
    (glBegin GL_QUAD_STRIP)
    (do ((i 0.0 (+ i 1.0))) ((>= i teeth))
      (let ((_angle (* i 2.0 (/ pi teeth)))
        (u 0.0)
        (v 0.0)
        (len 0.0))
    (glVertex3f (* r1 (cos _angle)) (* r1 (sin _angle)) (* width 0.5))
    (glVertex3f (* r1 (cos _angle)) (* r1 (sin _angle)) (* width -0.5))

    (set! u (- (* r2 (cos (+ _angle da))) (* r1 (cos _angle))))
    (set! v (- (* r2 (sin (+ _angle da))) (* r1 (sin _angle))))
    (set! len (sqrt (+ (* u u) (* v v))))
    ;; canonicalize normal vector
    (set! u (/ u len))
    (set! v (/ v len))

    (glNormal3f v (- u) 0.0)

    (glVertex3f (* r2 (cos (+ _angle da)))
            (* r2 (sin (+ _angle da)))
            (* width 0.5))
    (glVertex3f (* r2 (cos (+ _angle da)))
            (* r2 (sin (+ _angle da)))
            (* width -0.5))
    (glNormal3f (cos _angle) (sin _angle) 0.0)
    (glVertex3f (* r2 (cos (+ _angle (* 2 da))))
            (* r2 (sin (+ _angle (* 2 da))))
            (* width 0.5))
    (glVertex3f (* r2 (cos (+ _angle (* 2 da))))
            (* r2 (sin (+ _angle (* 2 da))))
            (* width -0.5))

    (set! u (- (* r1 (cos (+ _angle (* 3 da))))
           (* r2 (cos (+ _angle (* 2 da))))))
    (set! v (- (* r1 (sin (+ _angle (* 3 da))))
           (* r2 (sin (+ _angle (* 2 da))))))

    (glNormal3f v (- u) 0.0)

    (glVertex3f (* r1 (cos (+ _angle (* 3 da))))
            (* r1 (sin (+ _angle (* 3 da))))
            (* width 0.5))
    (glVertex3f (* r1 (cos (+ _angle (* 3 da))))
            (* r1 (sin (+ _angle (* 3 da))))
            (* width -0.5))
    (glNormal3f (cos _angle) (sin _angle) 0.0)))
    (glVertex3f (* r1 (cos 0.0)) (* r1 (sin 0.0)) (* width 0.5))
    (glVertex3f (* r1 (cos 0.0)) (* r1 (sin 0.0)) (* width -0.5))
    (glEnd)

    (glShadeModel GL_SMOOTH)

    ;; draw inside radius cylinder
    (glBegin GL_QUAD_STRIP)
    (do ((i 0.0 (+ i 1.0))) ((>= i (+ teeth 1.0)))
      (let ((_angle (* i 2.0 (/ pi teeth))))
    (glNormal3f (- (cos _angle)) (- (sin _angle)) 0.0)
    (glVertex3f (* r0 (cos _angle)) (* r0 (sin _angle)) (* width -0.5))
    (glVertex3f (* r0 (cos _angle)) (* r0 (sin _angle)) (* width 0.5))))
    (glEnd)
    ))

(define *view-rotx* 20.0)
(define *view-roty* 30.0)
(define *view-rotz*  0.0)
(define *gear1*  #f)
(define *gear2*  #f)
(define *gear3*  #f)
(define *angle*  0.0)
(define *frames* 0)
(define *t0*     0)
(define *win*    #f)

(define (cleanup)
  (glDeleteLists *gear1* 1)
  (glDeleteLists *gear2* 1)
  (glDeleteLists *gear3* 1)
  (glutDestroyWindow *win*))

(define (draw)
  ;;*** OpenGL BEGIN ***
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (begin
    (glPushMatrix)
    (glRotatef *view-rotx* 1.0 0.0 0.0)
    (glRotatef *view-roty* 0.0 1.0 0.0)
    (glRotatef *view-rotz* 0.0 0.0 1.0)
    (begin
      (glPushMatrix)
      (glTranslatef -3.0 -2.0 0.0)
      (glRotatef *angle* 0.0 0.0 1.0)
      (glCallList *gear1*)
      (glPopMatrix))
    (begin
      (glPushMatrix)
      (glTranslatef 3.1 -2.0 0.0)
      (glRotatef (- (* -2.0 *angle*) 9.0) 0.0 0.0 1.0)
      (glCallList *gear2*)
      (glPopMatrix))
    (begin
      (glPushMatrix)
      (glTranslatef -3.1 4.2 0.0)
      (glRotatef (- (* -2.0 *angle*) 25.0) 0.0 0.0 1.0)
      (glCallList *gear3*)
      (glPopMatrix))
    (glPopMatrix))

  (glutSwapBuffers)

  (set! *frames* (+ 1 *frames*))

  (let ((t (glutGet GLUT_ELAPSED_TIME)))
    (when (>= (- t *t0*) 5000)
      (let ((seconds (/ (- t *t0*) 1000.0)))
        (format #t "~d in ~d seconds = ~d FPS~%" *frames* seconds (/ *frames* seconds))
        (set! *t0*     t)
        (set! *frames* 0)))))

;; new window size or exposure
(define (reshape width height)
  (let ((h (/. height width)))
    ;;*** OpenGL BEGIN ***
    (glViewport 0 0 width height)
    (glMatrixMode GL_PROJECTION)
    (glLoadIdentity)
    (glFrustum -1.0 1.0 (- h) h 5.0 60.0)
    (glMatrixMode GL_MODELVIEW)
    (glLoadIdentity)
    (glTranslatef 0.0 0.0 -40.0)
    ;;*** OpenGL END ***
    ))

(define (init)
  ;;*** OpenGL BEGIN ***
  (glLightfv GL_LIGHT0 GL_POSITION (f32vector 5.0 5.0 10.0 0.0))
  (glEnable GL_CULL_FACE)
  (glEnable GL_LIGHTING)
  (glEnable GL_LIGHT0)
  (glEnable GL_DEPTH_TEST)

  ;; make the gears
  (set! *gear1* (glGenLists 1))
  (glNewList *gear1* GL_COMPILE)
  (glMaterialfv GL_FRONT GL_AMBIENT_AND_DIFFUSE (f32vector 0.8 0.1 0.0 1.0))
  (gear 1.0 4.0 1.0 20 0.7)
  (glEndList)

  (set! *gear2* (glGenLists 1))
  (glNewList *gear2* GL_COMPILE)
  (glMaterialfv GL_FRONT GL_AMBIENT_AND_DIFFUSE (f32vector 0.0 0.8 0.2 1.0))
  (gear 0.5 2.0 2.0 10 0.7)
  (glEndList)

  (set! *gear3* (glGenLists 1))
  (glNewList *gear3* GL_COMPILE)
  (glMaterialfv GL_FRONT GL_AMBIENT_AND_DIFFUSE (f32vector 0.2 0.2 1.0 1.0))
  (gear 1.3 2.0 0.5 10 0.7)
  (glEndList)
  
  (glEnable GL_NORMALIZE)

  (format #t "GL_RENDERER   = ~s~%" (glGetString GL_RENDERER))
  (format #t "GL_VERSION    = ~s~%" (glGetString GL_VERSION))
  (format #t "GL_VENDOR     = ~s~%" (glGetString GL_VENDOR))
  (format #t "GL_EXTENSIONS = ~s~%" (glGetString GL_EXTENSIONS))
  (newline)
  ;;*** OpenGL END ***
  )

(define idle
  (let ((t0 #f))
    (lambda ()
      (let ((dt #f)
        (t (/ (glutGet GLUT_ELAPSED_TIME) 1000.0)))
    (unless (number? t0)
        (set! t0 t))
    (set! dt (- t t0))
    (set! t0 t)
    (set! *angle* (+ *angle* (* 70.0 dt))) ; 70 degrees per second
    (set! *angle* (mod *angle* 360.0)) ; prevents eventual overflow
    (glutPostRedisplay)))))

;; change view angle, exit upon ESC 
(define (key rk x y)
  (let ((q (lambda () (glutPostRedisplay)))
    (k (c-int->c-uchar rk)))
    (cond
     ((= k (char->integer #\z))
      (set! *view-rotz* (mod (+ *view-rotz* 5.0) 360.0)) (q))
     ((= k (char->integer #\Z))
      (set! *view-rotz* (mod (- *view-rotz* 5.0) 360.0)) (q))
     ((= k (char->integer #\esc)) (exit)))))

;; change view angle
(define (special k x y)
  (let ((q (lambda () (glutPostRedisplay))))
    (cond
     ((= k GLUT_KEY_UP)
      (set! *view-rotx* (mod (+ *view-rotx* 5.0) 360.0)) (q))
     ((= k GLUT_KEY_DOWN)
      (set! *view-rotx* (mod (- *view-rotx* 5.0) 360.0)) (q))
     ((= k GLUT_KEY_LEFT)
      (set! *view-roty* (mod (+ *view-roty* 5.0) 360.0)) (q))
     ((= k GLUT_KEY_RIGHT)
      (set! *view-roty* (mod (- *view-roty* 5.0) 360.0)) (q)))))

(define (visible vis)
  (if (= vis GLUT_VISIBLE)
      (glutIdleFunc idle)
      (glutIdleFunc (lambda () (usleep 100000)))))

(begin
  (glutInit (vector (length (command-line))) (apply vector (command-line)))
  (glutInitDisplayMode (bitwise-ior GLUT_DOUBLE GLUT_DEPTH GLUT_RGB))

  (glutInitWindowPosition   0   0)
  (glutInitWindowSize     300 300)

  (set! *win* (glutCreateWindow "Gears"))
  (init)

  (glutDisplayFunc    draw)
  (glutReshapeFunc    reshape)
  (glutKeyboardFunc   key)
  (glutSpecialFunc    special)
  (glutVisibilityFunc visible)

  (glutMainLoop)
  )

;; end
