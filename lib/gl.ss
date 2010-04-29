;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2008 Y.FUJITA, LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (gl)

  (export GL_FALSE
          GL_TRUE
          GL_BYTE
          GL_UNSIGNED_BYTE
          GL_SHORT
          GL_UNSIGNED_SHORT
          GL_INT
          GL_UNSIGNED_INT
          GL_FLOAT
          GL_2_BYTES
          GL_3_BYTES
          GL_4_BYTES
          GL_DOUBLE
          GL_POINTS
          GL_LINES
          GL_LINE_LOOP
          GL_LINE_STRIP
          GL_TRIANGLES
          GL_TRIANGLE_STRIP
          GL_TRIANGLE_FAN
          GL_QUADS
          GL_QUAD_STRIP
          GL_POLYGON
          GL_VERTEX_ARRAY
          GL_NORMAL_ARRAY
          GL_COLOR_ARRAY
          GL_INDEX_ARRAY
          GL_TEXTURE_COORD_ARRAY
          GL_EDGE_FLAG_ARRAY
          GL_VERTEX_ARRAY_SIZE
          GL_VERTEX_ARRAY_TYPE
          GL_VERTEX_ARRAY_STRIDE
          GL_NORMAL_ARRAY_TYPE
          GL_NORMAL_ARRAY_STRIDE
          GL_COLOR_ARRAY_SIZE
          GL_COLOR_ARRAY_TYPE
          GL_COLOR_ARRAY_STRIDE
          GL_INDEX_ARRAY_TYPE
          GL_INDEX_ARRAY_STRIDE
          GL_TEXTURE_COORD_ARRAY_SIZE
          GL_TEXTURE_COORD_ARRAY_TYPE
          GL_TEXTURE_COORD_ARRAY_STRIDE
          GL_EDGE_FLAG_ARRAY_STRIDE
          GL_VERTEX_ARRAY_POINTER
          GL_NORMAL_ARRAY_POINTER
          GL_COLOR_ARRAY_POINTER
          GL_INDEX_ARRAY_POINTER
          GL_TEXTURE_COORD_ARRAY_POINTER
          GL_EDGE_FLAG_ARRAY_POINTER
          GL_V2F
          GL_V3F
          GL_C4UB_V2F
          GL_C4UB_V3F
          GL_C3F_V3F
          GL_N3F_V3F
          GL_C4F_N3F_V3F
          GL_T2F_V3F
          GL_T4F_V4F
          GL_T2F_C4UB_V3F
          GL_T2F_C3F_V3F
          GL_T2F_N3F_V3F
          GL_T2F_C4F_N3F_V3F
          GL_T4F_C4F_N3F_V4F
          GL_MATRIX_MODE
          GL_MODELVIEW
          GL_PROJECTION
          GL_TEXTURE
          GL_POINT_SMOOTH
          GL_POINT_SIZE
          GL_POINT_SIZE_GRANULARITY
          GL_POINT_SIZE_RANGE
          GL_LINE_SMOOTH
          GL_LINE_STIPPLE
          GL_LINE_STIPPLE_PATTERN
          GL_LINE_STIPPLE_REPEAT
          GL_LINE_WIDTH
          GL_LINE_WIDTH_GRANULARITY
          GL_LINE_WIDTH_RANGE
          GL_POINT
          GL_LINE
          GL_FILL
          GL_CW
          GL_CCW
          GL_FRONT
          GL_BACK
          GL_POLYGON_MODE
          GL_POLYGON_SMOOTH
          GL_POLYGON_STIPPLE
          GL_EDGE_FLAG
          GL_CULL_FACE
          GL_CULL_FACE_MODE
          GL_FRONT_FACE
          GL_POLYGON_OFFSET_FACTOR
          GL_POLYGON_OFFSET_UNITS
          GL_POLYGON_OFFSET_POINT
          GL_POLYGON_OFFSET_LINE
          GL_POLYGON_OFFSET_FILL
          GL_COMPILE
          GL_COMPILE_AND_EXECUTE
          GL_LIST_BASE
          GL_LIST_INDEX
          GL_LIST_MODE
          GL_NEVER
          GL_LESS
          GL_EQUAL
          GL_LEQUAL
          GL_GREATER
          GL_NOTEQUAL
          GL_GEQUAL
          GL_ALWAYS
          GL_DEPTH_TEST
          GL_DEPTH_BITS
          GL_DEPTH_CLEAR_VALUE
          GL_DEPTH_FUNC
          GL_DEPTH_RANGE
          GL_DEPTH_WRITEMASK
          GL_DEPTH_COMPONENT
          GL_LIGHTING
          GL_LIGHT0
          GL_LIGHT1
          GL_LIGHT2
          GL_LIGHT3
          GL_LIGHT4
          GL_LIGHT5
          GL_LIGHT6
          GL_LIGHT7
          GL_SPOT_EXPONENT
          GL_SPOT_CUTOFF
          GL_CONSTANT_ATTENUATION
          GL_LINEAR_ATTENUATION
          GL_QUADRATIC_ATTENUATION
          GL_AMBIENT
          GL_DIFFUSE
          GL_SPECULAR
          GL_SHININESS
          GL_EMISSION
          GL_POSITION
          GL_SPOT_DIRECTION
          GL_AMBIENT_AND_DIFFUSE
          GL_COLOR_INDEXES
          GL_LIGHT_MODEL_TWO_SIDE
          GL_LIGHT_MODEL_LOCAL_VIEWER
          GL_LIGHT_MODEL_AMBIENT
          GL_FRONT_AND_BACK
          GL_SHADE_MODEL
          GL_FLAT
          GL_SMOOTH
          GL_COLOR_MATERIAL
          GL_COLOR_MATERIAL_FACE
          GL_COLOR_MATERIAL_PARAMETER
          GL_NORMALIZE
          GL_CLIP_PLANE0
          GL_CLIP_PLANE1
          GL_CLIP_PLANE2
          GL_CLIP_PLANE3
          GL_CLIP_PLANE4
          GL_CLIP_PLANE5
          GL_ACCUM_RED_BITS
          GL_ACCUM_GREEN_BITS
          GL_ACCUM_BLUE_BITS
          GL_ACCUM_ALPHA_BITS
          GL_ACCUM_CLEAR_VALUE
          GL_ACCUM
          GL_ADD
          GL_LOAD
          GL_MULT
          GL_RETURN
          GL_ALPHA_TEST
          GL_ALPHA_TEST_REF
          GL_ALPHA_TEST_FUNC
          GL_BLEND
          GL_BLEND_SRC
          GL_BLEND_DST
          GL_ZERO
          GL_ONE
          GL_SRC_COLOR
          GL_ONE_MINUS_SRC_COLOR
          GL_SRC_ALPHA
          GL_ONE_MINUS_SRC_ALPHA
          GL_DST_ALPHA
          GL_ONE_MINUS_DST_ALPHA
          GL_DST_COLOR
          GL_ONE_MINUS_DST_COLOR
          GL_SRC_ALPHA_SATURATE
          GL_FEEDBACK
          GL_RENDER
          GL_SELECT
          GL_2D
          GL_3D
          GL_3D_COLOR
          GL_3D_COLOR_TEXTURE
          GL_4D_COLOR_TEXTURE
          GL_POINT_TOKEN
          GL_LINE_TOKEN
          GL_LINE_RESET_TOKEN
          GL_POLYGON_TOKEN
          GL_BITMAP_TOKEN
          GL_DRAW_PIXEL_TOKEN
          GL_COPY_PIXEL_TOKEN
          GL_PASS_THROUGH_TOKEN
          GL_FEEDBACK_BUFFER_POINTER
          GL_FEEDBACK_BUFFER_SIZE
          GL_FEEDBACK_BUFFER_TYPE
          GL_SELECTION_BUFFER_POINTER
          GL_SELECTION_BUFFER_SIZE
          GL_FOG
          GL_FOG_MODE
          GL_FOG_DENSITY
          GL_FOG_COLOR
          GL_FOG_INDEX
          GL_FOG_START
          GL_FOG_END
          GL_LINEAR
          GL_EXP
          GL_EXP2
          GL_LOGIC_OP
          GL_INDEX_LOGIC_OP
          GL_COLOR_LOGIC_OP
          GL_LOGIC_OP_MODE
          GL_CLEAR
          GL_SET
          GL_COPY
          GL_COPY_INVERTED
          GL_NOOP
          GL_INVERT
          GL_AND
          GL_NAND
          GL_OR
          GL_NOR
          GL_XOR
          GL_EQUIV
          GL_AND_REVERSE
          GL_AND_INVERTED
          GL_OR_REVERSE
          GL_OR_INVERTED
          GL_STENCIL_BITS
          GL_STENCIL_TEST
          GL_STENCIL_CLEAR_VALUE
          GL_STENCIL_FUNC
          GL_STENCIL_VALUE_MASK
          GL_STENCIL_FAIL
          GL_STENCIL_PASS_DEPTH_FAIL
          GL_STENCIL_PASS_DEPTH_PASS
          GL_STENCIL_REF
          GL_STENCIL_WRITEMASK
          GL_STENCIL_INDEX
          GL_KEEP
          GL_REPLACE
          GL_INCR
          GL_DECR
          GL_NONE
          GL_LEFT
          GL_RIGHT
          GL_FRONT_LEFT
          GL_FRONT_RIGHT
          GL_BACK_LEFT
          GL_BACK_RIGHT
          GL_AUX0
          GL_AUX1
          GL_AUX2
          GL_AUX3
          GL_COLOR_INDEX
          GL_RED
          GL_GREEN
          GL_BLUE
          GL_ALPHA
          GL_LUMINANCE
          GL_LUMINANCE_ALPHA
          GL_ALPHA_BITS
          GL_RED_BITS
          GL_GREEN_BITS
          GL_BLUE_BITS
          GL_INDEX_BITS
          GL_SUBPIXEL_BITS
          GL_AUX_BUFFERS
          GL_READ_BUFFER
          GL_DRAW_BUFFER
          GL_DOUBLEBUFFER
          GL_STEREO
          GL_BITMAP
          GL_COLOR
          GL_DEPTH
          GL_STENCIL
          GL_DITHER
          GL_RGB
          GL_RGBA
          GL_MAX_LIST_NESTING
          GL_MAX_EVAL_ORDER
          GL_MAX_LIGHTS
          GL_MAX_CLIP_PLANES
          GL_MAX_TEXTURE_SIZE
          GL_MAX_PIXEL_MAP_TABLE
          GL_MAX_ATTRIB_STACK_DEPTH
          GL_MAX_MODELVIEW_STACK_DEPTH
          GL_MAX_NAME_STACK_DEPTH
          GL_MAX_PROJECTION_STACK_DEPTH
          GL_MAX_TEXTURE_STACK_DEPTH
          GL_MAX_VIEWPORT_DIMS
          GL_MAX_CLIENT_ATTRIB_STACK_DEPTH
          GL_ATTRIB_STACK_DEPTH
          GL_CLIENT_ATTRIB_STACK_DEPTH
          GL_COLOR_CLEAR_VALUE
          GL_COLOR_WRITEMASK
          GL_CURRENT_INDEX
          GL_CURRENT_COLOR
          GL_CURRENT_NORMAL
          GL_CURRENT_RASTER_COLOR
          GL_CURRENT_RASTER_DISTANCE
          GL_CURRENT_RASTER_INDEX
          GL_CURRENT_RASTER_POSITION
          GL_CURRENT_RASTER_TEXTURE_COORDS
          GL_CURRENT_RASTER_POSITION_VALID
          GL_CURRENT_TEXTURE_COORDS
          GL_INDEX_CLEAR_VALUE
          GL_INDEX_MODE
          GL_INDEX_WRITEMASK
          GL_MODELVIEW_MATRIX
          GL_MODELVIEW_STACK_DEPTH
          GL_NAME_STACK_DEPTH
          GL_PROJECTION_MATRIX
          GL_PROJECTION_STACK_DEPTH
          GL_RENDER_MODE
          GL_RGBA_MODE
          GL_TEXTURE_MATRIX
          GL_TEXTURE_STACK_DEPTH
          GL_VIEWPORT
          GL_AUTO_NORMAL
          GL_MAP1_COLOR_4
          GL_MAP1_INDEX
          GL_MAP1_NORMAL
          GL_MAP1_TEXTURE_COORD_1
          GL_MAP1_TEXTURE_COORD_2
          GL_MAP1_TEXTURE_COORD_3
          GL_MAP1_TEXTURE_COORD_4
          GL_MAP1_VERTEX_3
          GL_MAP1_VERTEX_4
          GL_MAP2_COLOR_4
          GL_MAP2_INDEX
          GL_MAP2_NORMAL
          GL_MAP2_TEXTURE_COORD_1
          GL_MAP2_TEXTURE_COORD_2
          GL_MAP2_TEXTURE_COORD_3
          GL_MAP2_TEXTURE_COORD_4
          GL_MAP2_VERTEX_3
          GL_MAP2_VERTEX_4
          GL_MAP1_GRID_DOMAIN
          GL_MAP1_GRID_SEGMENTS
          GL_MAP2_GRID_DOMAIN
          GL_MAP2_GRID_SEGMENTS
          GL_COEFF
          GL_ORDER
          GL_DOMAIN
          GL_PERSPECTIVE_CORRECTION_HINT
          GL_POINT_SMOOTH_HINT
          GL_LINE_SMOOTH_HINT
          GL_POLYGON_SMOOTH_HINT
          GL_FOG_HINT
          GL_DONT_CARE
          GL_FASTEST
          GL_NICEST
          GL_SCISSOR_BOX
          GL_SCISSOR_TEST
          GL_MAP_COLOR
          GL_MAP_STENCIL
          GL_INDEX_SHIFT
          GL_INDEX_OFFSET
          GL_RED_SCALE
          GL_RED_BIAS
          GL_GREEN_SCALE
          GL_GREEN_BIAS
          GL_BLUE_SCALE
          GL_BLUE_BIAS
          GL_ALPHA_SCALE
          GL_ALPHA_BIAS
          GL_DEPTH_SCALE
          GL_DEPTH_BIAS
          GL_PIXEL_MAP_S_TO_S_SIZE
          GL_PIXEL_MAP_I_TO_I_SIZE
          GL_PIXEL_MAP_I_TO_R_SIZE
          GL_PIXEL_MAP_I_TO_G_SIZE
          GL_PIXEL_MAP_I_TO_B_SIZE
          GL_PIXEL_MAP_I_TO_A_SIZE
          GL_PIXEL_MAP_R_TO_R_SIZE
          GL_PIXEL_MAP_G_TO_G_SIZE
          GL_PIXEL_MAP_B_TO_B_SIZE
          GL_PIXEL_MAP_A_TO_A_SIZE
          GL_PIXEL_MAP_S_TO_S
          GL_PIXEL_MAP_I_TO_I
          GL_PIXEL_MAP_I_TO_R
          GL_PIXEL_MAP_I_TO_G
          GL_PIXEL_MAP_I_TO_B
          GL_PIXEL_MAP_I_TO_A
          GL_PIXEL_MAP_R_TO_R
          GL_PIXEL_MAP_G_TO_G
          GL_PIXEL_MAP_B_TO_B
          GL_PIXEL_MAP_A_TO_A
          GL_PACK_ALIGNMENT
          GL_PACK_LSB_FIRST
          GL_PACK_ROW_LENGTH
          GL_PACK_SKIP_PIXELS
          GL_PACK_SKIP_ROWS
          GL_PACK_SWAP_BYTES
          GL_UNPACK_ALIGNMENT
          GL_UNPACK_LSB_FIRST
          GL_UNPACK_ROW_LENGTH
          GL_UNPACK_SKIP_PIXELS
          GL_UNPACK_SKIP_ROWS
          GL_UNPACK_SWAP_BYTES
          GL_ZOOM_X
          GL_ZOOM_Y
          GL_TEXTURE_ENV
          GL_TEXTURE_ENV_MODE
          GL_TEXTURE_1D
          GL_TEXTURE_2D
          GL_TEXTURE_WRAP_S
          GL_TEXTURE_WRAP_T
          GL_TEXTURE_MAG_FILTER
          GL_TEXTURE_MIN_FILTER
          GL_TEXTURE_ENV_COLOR
          GL_TEXTURE_GEN_S
          GL_TEXTURE_GEN_T
          GL_TEXTURE_GEN_MODE
          GL_TEXTURE_BORDER_COLOR
          GL_TEXTURE_WIDTH
          GL_TEXTURE_HEIGHT
          GL_TEXTURE_BORDER
          GL_TEXTURE_COMPONENTS
          GL_TEXTURE_RED_SIZE
          GL_TEXTURE_GREEN_SIZE
          GL_TEXTURE_BLUE_SIZE
          GL_TEXTURE_ALPHA_SIZE
          GL_TEXTURE_LUMINANCE_SIZE
          GL_TEXTURE_INTENSITY_SIZE
          GL_NEAREST_MIPMAP_NEAREST
          GL_NEAREST_MIPMAP_LINEAR
          GL_LINEAR_MIPMAP_NEAREST
          GL_LINEAR_MIPMAP_LINEAR
          GL_OBJECT_LINEAR
          GL_OBJECT_PLANE
          GL_EYE_LINEAR
          GL_EYE_PLANE
          GL_SPHERE_MAP
          GL_DECAL
          GL_MODULATE
          GL_NEAREST
          GL_REPEAT
          GL_CLAMP
          GL_S
          GL_T
          GL_R
          GL_Q
          GL_TEXTURE_GEN_R
          GL_TEXTURE_GEN_Q
          GL_VENDOR
          GL_RENDERER
          GL_VERSION
          GL_EXTENSIONS
          GL_NO_ERROR
          GL_INVALID_ENUM
          GL_INVALID_VALUE
          GL_INVALID_OPERATION
          GL_STACK_OVERFLOW
          GL_STACK_UNDERFLOW
          GL_OUT_OF_MEMORY
          GL_CURRENT_BIT
          GL_POINT_BIT
          GL_LINE_BIT
          GL_POLYGON_BIT
          GL_POLYGON_STIPPLE_BIT
          GL_PIXEL_MODE_BIT
          GL_LIGHTING_BIT
          GL_FOG_BIT
          GL_DEPTH_BUFFER_BIT
          GL_ACCUM_BUFFER_BIT
          GL_STENCIL_BUFFER_BIT
          GL_VIEWPORT_BIT
          GL_TRANSFORM_BIT
          GL_ENABLE_BIT
          GL_COLOR_BUFFER_BIT
          GL_HINT_BIT
          GL_EVAL_BIT
          GL_LIST_BIT
          GL_TEXTURE_BIT
          GL_SCISSOR_BIT
          GL_ALL_ATTRIB_BITS
          GL_PROXY_TEXTURE_1D
          GL_PROXY_TEXTURE_2D
          GL_TEXTURE_PRIORITY
          GL_TEXTURE_RESIDENT
          GL_TEXTURE_BINDING_1D
          GL_TEXTURE_BINDING_2D
          GL_TEXTURE_INTERNAL_FORMAT
          GL_ALPHA4
          GL_ALPHA8
          GL_ALPHA12
          GL_ALPHA16
          GL_LUMINANCE4
          GL_LUMINANCE8
          GL_LUMINANCE12
          GL_LUMINANCE16
          GL_LUMINANCE4_ALPHA4
          GL_LUMINANCE6_ALPHA2
          GL_LUMINANCE8_ALPHA8
          GL_LUMINANCE12_ALPHA4
          GL_LUMINANCE12_ALPHA12
          GL_LUMINANCE16_ALPHA16
          GL_INTENSITY
          GL_INTENSITY4
          GL_INTENSITY8
          GL_INTENSITY12
          GL_INTENSITY16
          GL_R3_G3_B2
          GL_RGB4
          GL_RGB5
          GL_RGB8
          GL_RGB10
          GL_RGB12
          GL_RGB16
          GL_RGBA2
          GL_RGBA4
          GL_RGB5_A1
          GL_RGBA8
          GL_RGB10_A2
          GL_RGBA12
          GL_RGBA16
          GL_CLIENT_PIXEL_STORE_BIT
          GL_CLIENT_VERTEX_ARRAY_BIT
          GL_ALL_CLIENT_ATTRIB_BITS
          GL_CLIENT_ALL_ATTRIB_BITS
          GL_RESCALE_NORMAL
          GL_CLAMP_TO_EDGE
          GL_MAX_ELEMENTS_VERTICES
          GL_MAX_ELEMENTS_INDICES
          GL_BGR
          GL_BGRA
          GL_UNSIGNED_BYTE_3_3_2
          GL_UNSIGNED_BYTE_2_3_3_REV
          GL_UNSIGNED_SHORT_5_6_5
          GL_UNSIGNED_SHORT_5_6_5_REV
          GL_UNSIGNED_SHORT_4_4_4_4
          GL_UNSIGNED_SHORT_4_4_4_4_REV
          GL_UNSIGNED_SHORT_5_5_5_1
          GL_UNSIGNED_SHORT_1_5_5_5_REV
          GL_UNSIGNED_INT_8_8_8_8
          GL_UNSIGNED_INT_8_8_8_8_REV
          GL_UNSIGNED_INT_10_10_10_2
          GL_UNSIGNED_INT_2_10_10_10_REV
          GL_LIGHT_MODEL_COLOR_CONTROL
          GL_SINGLE_COLOR
          GL_SEPARATE_SPECULAR_COLOR
          GL_TEXTURE_MIN_LOD
          GL_TEXTURE_MAX_LOD
          GL_TEXTURE_BASE_LEVEL
          GL_TEXTURE_MAX_LEVEL
          GL_SMOOTH_POINT_SIZE_RANGE
          GL_SMOOTH_POINT_SIZE_GRANULARITY
          GL_SMOOTH_LINE_WIDTH_RANGE
          GL_SMOOTH_LINE_WIDTH_GRANULARITY
          GL_ALIASED_POINT_SIZE_RANGE
          GL_ALIASED_LINE_WIDTH_RANGE
          GL_PACK_SKIP_IMAGES
          GL_PACK_IMAGE_HEIGHT
          GL_UNPACK_SKIP_IMAGES
          GL_UNPACK_IMAGE_HEIGHT
          GL_TEXTURE_3D
          GL_PROXY_TEXTURE_3D
          GL_TEXTURE_DEPTH
          GL_TEXTURE_WRAP_R
          GL_MAX_3D_TEXTURE_SIZE
          GL_TEXTURE_BINDING_3D
          GL_CONSTANT_COLOR
          GL_ONE_MINUS_CONSTANT_COLOR
          GL_CONSTANT_ALPHA
          GL_ONE_MINUS_CONSTANT_ALPHA
          GL_COLOR_TABLE
          GL_POST_CONVOLUTION_COLOR_TABLE
          GL_POST_COLOR_MATRIX_COLOR_TABLE
          GL_PROXY_COLOR_TABLE
          GL_PROXY_POST_CONVOLUTION_COLOR_TABLE
          GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE
          GL_COLOR_TABLE_SCALE
          GL_COLOR_TABLE_BIAS
          GL_COLOR_TABLE_FORMAT
          GL_COLOR_TABLE_WIDTH
          GL_COLOR_TABLE_RED_SIZE
          GL_COLOR_TABLE_GREEN_SIZE
          GL_COLOR_TABLE_BLUE_SIZE
          GL_COLOR_TABLE_ALPHA_SIZE
          GL_COLOR_TABLE_LUMINANCE_SIZE
          GL_COLOR_TABLE_INTENSITY_SIZE
          GL_CONVOLUTION_1D
          GL_CONVOLUTION_2D
          GL_SEPARABLE_2D
          GL_CONVOLUTION_BORDER_MODE
          GL_CONVOLUTION_FILTER_SCALE
          GL_CONVOLUTION_FILTER_BIAS
          GL_REDUCE
          GL_CONVOLUTION_FORMAT
          GL_CONVOLUTION_WIDTH
          GL_CONVOLUTION_HEIGHT
          GL_MAX_CONVOLUTION_WIDTH
          GL_MAX_CONVOLUTION_HEIGHT
          GL_POST_CONVOLUTION_RED_SCALE
          GL_POST_CONVOLUTION_GREEN_SCALE
          GL_POST_CONVOLUTION_BLUE_SCALE
          GL_POST_CONVOLUTION_ALPHA_SCALE
          GL_POST_CONVOLUTION_RED_BIAS
          GL_POST_CONVOLUTION_GREEN_BIAS
          GL_POST_CONVOLUTION_BLUE_BIAS
          GL_POST_CONVOLUTION_ALPHA_BIAS
          GL_CONSTANT_BORDER
          GL_REPLICATE_BORDER
          GL_CONVOLUTION_BORDER_COLOR
          GL_COLOR_MATRIX
          GL_COLOR_MATRIX_STACK_DEPTH
          GL_MAX_COLOR_MATRIX_STACK_DEPTH
          GL_POST_COLOR_MATRIX_RED_SCALE
          GL_POST_COLOR_MATRIX_GREEN_SCALE
          GL_POST_COLOR_MATRIX_BLUE_SCALE
          GL_POST_COLOR_MATRIX_ALPHA_SCALE
          GL_POST_COLOR_MATRIX_RED_BIAS
          GL_POST_COLOR_MATRIX_GREEN_BIAS
          GL_POST_COLOR_MATRIX_BLUE_BIAS
          GL_POST_COLOR_MATRIX_ALPHA_BIAS
          GL_HISTOGRAM
          GL_PROXY_HISTOGRAM
          GL_HISTOGRAM_WIDTH
          GL_HISTOGRAM_FORMAT
          GL_HISTOGRAM_RED_SIZE
          GL_HISTOGRAM_GREEN_SIZE
          GL_HISTOGRAM_BLUE_SIZE
          GL_HISTOGRAM_ALPHA_SIZE
          GL_HISTOGRAM_LUMINANCE_SIZE
          GL_HISTOGRAM_SINK
          GL_MINMAX
          GL_MINMAX_FORMAT
          GL_MINMAX_SINK
          GL_TABLE_TOO_LARGE
          GL_BLEND_EQUATION
          GL_MIN
          GL_MAX
          GL_FUNC_ADD
          GL_FUNC_SUBTRACT
          GL_FUNC_REVERSE_SUBTRACT
          GL_BLEND_COLOR
          GL_TEXTURE0
          GL_TEXTURE1
          GL_TEXTURE2
          GL_TEXTURE3
          GL_TEXTURE4
          GL_TEXTURE5
          GL_TEXTURE6
          GL_TEXTURE7
          GL_TEXTURE8
          GL_TEXTURE9
          GL_TEXTURE10
          GL_TEXTURE11
          GL_TEXTURE12
          GL_TEXTURE13
          GL_TEXTURE14
          GL_TEXTURE15
          GL_TEXTURE16
          GL_TEXTURE17
          GL_TEXTURE18
          GL_TEXTURE19
          GL_TEXTURE20
          GL_TEXTURE21
          GL_TEXTURE22
          GL_TEXTURE23
          GL_TEXTURE24
          GL_TEXTURE25
          GL_TEXTURE26
          GL_TEXTURE27
          GL_TEXTURE28
          GL_TEXTURE29
          GL_TEXTURE30
          GL_TEXTURE31
          GL_ACTIVE_TEXTURE
          GL_CLIENT_ACTIVE_TEXTURE
          GL_MAX_TEXTURE_UNITS
          GL_NORMAL_MAP
          GL_REFLECTION_MAP
          GL_TEXTURE_CUBE_MAP
          GL_TEXTURE_BINDING_CUBE_MAP
          GL_TEXTURE_CUBE_MAP_POSITIVE_X
          GL_TEXTURE_CUBE_MAP_NEGATIVE_X
          GL_TEXTURE_CUBE_MAP_POSITIVE_Y
          GL_TEXTURE_CUBE_MAP_NEGATIVE_Y
          GL_TEXTURE_CUBE_MAP_POSITIVE_Z
          GL_TEXTURE_CUBE_MAP_NEGATIVE_Z
          GL_PROXY_TEXTURE_CUBE_MAP
          GL_MAX_CUBE_MAP_TEXTURE_SIZE
          GL_COMPRESSED_ALPHA
          GL_COMPRESSED_LUMINANCE
          GL_COMPRESSED_LUMINANCE_ALPHA
          GL_COMPRESSED_INTENSITY
          GL_COMPRESSED_RGB
          GL_COMPRESSED_RGBA
          GL_TEXTURE_COMPRESSION_HINT
          GL_TEXTURE_COMPRESSED_IMAGE_SIZE
          GL_TEXTURE_COMPRESSED
          GL_NUM_COMPRESSED_TEXTURE_FORMATS
          GL_COMPRESSED_TEXTURE_FORMATS
          GL_MULTISAMPLE
          GL_SAMPLE_ALPHA_TO_COVERAGE
          GL_SAMPLE_ALPHA_TO_ONE
          GL_SAMPLE_COVERAGE
          GL_SAMPLE_BUFFERS
          GL_SAMPLES
          GL_SAMPLE_COVERAGE_VALUE
          GL_SAMPLE_COVERAGE_INVERT
          GL_MULTISAMPLE_BIT
          GL_TRANSPOSE_MODELVIEW_MATRIX
          GL_TRANSPOSE_PROJECTION_MATRIX
          GL_TRANSPOSE_TEXTURE_MATRIX
          GL_TRANSPOSE_COLOR_MATRIX
          GL_COMBINE
          GL_COMBINE_RGB
          GL_COMBINE_ALPHA
          GL_SOURCE0_RGB
          GL_SOURCE1_RGB
          GL_SOURCE2_RGB
          GL_SOURCE0_ALPHA
          GL_SOURCE1_ALPHA
          GL_SOURCE2_ALPHA
          GL_OPERAND0_RGB
          GL_OPERAND1_RGB
          GL_OPERAND2_RGB
          GL_OPERAND0_ALPHA
          GL_OPERAND1_ALPHA
          GL_OPERAND2_ALPHA
          GL_RGB_SCALE
          GL_ADD_SIGNED
          GL_INTERPOLATE
          GL_SUBTRACT
          GL_CONSTANT
          GL_PRIMARY_COLOR
          GL_PREVIOUS
          GL_DOT3_RGB
          GL_DOT3_RGBA
          GL_CLAMP_TO_BORDER
          GL_TEXTURE0_ARB
          GL_TEXTURE1_ARB
          GL_TEXTURE2_ARB
          GL_TEXTURE3_ARB
          GL_TEXTURE4_ARB
          GL_TEXTURE5_ARB
          GL_TEXTURE6_ARB
          GL_TEXTURE7_ARB
          GL_TEXTURE8_ARB
          GL_TEXTURE9_ARB
          GL_TEXTURE10_ARB
          GL_TEXTURE11_ARB
          GL_TEXTURE12_ARB
          GL_TEXTURE13_ARB
          GL_TEXTURE14_ARB
          GL_TEXTURE15_ARB
          GL_TEXTURE16_ARB
          GL_TEXTURE17_ARB
          GL_TEXTURE18_ARB
          GL_TEXTURE19_ARB
          GL_TEXTURE20_ARB
          GL_TEXTURE21_ARB
          GL_TEXTURE22_ARB
          GL_TEXTURE23_ARB
          GL_TEXTURE24_ARB
          GL_TEXTURE25_ARB
          GL_TEXTURE26_ARB
          GL_TEXTURE27_ARB
          GL_TEXTURE28_ARB
          GL_TEXTURE29_ARB
          GL_TEXTURE30_ARB
          GL_TEXTURE31_ARB
          GL_ACTIVE_TEXTURE_ARB
          GL_CLIENT_ACTIVE_TEXTURE_ARB
          GL_MAX_TEXTURE_UNITS_ARB

          glClearIndex
          glClearColor
          glClear
          glIndexMask
          glColorMask
          glAlphaFunc
          glBlendFunc
          glLogicOp
          glCullFace
          glFrontFace
          glPointSize
          glLineWidth
          glLineStipple
          glPolygonMode
          glPolygonOffset
          glPolygonStipple
          glGetPolygonStipple
          glEdgeFlag
          glEdgeFlagv
          glScissor
          glClipPlane
          glGetClipPlane
          glDrawBuffer
          glReadBuffer
          glEnable
          glDisable
          glIsEnabled
          glEnableClientState
          glDisableClientState
          glGetBooleanv
          glGetDoublev
          glGetFloatv
          glGetIntegerv
          glPushAttrib
          glPopAttrib
          glPushClientAttrib
          glPopClientAttrib
          glRenderMode
          glGetError
          glGetString
          glFinish
          glFlush
          glHint
          glClearDepth
          glDepthFunc
          glDepthMask
          glDepthRange
          glClearAccum
          glAccum
          glMatrixMode
          glOrtho
          glFrustum
          glViewport
          glPushMatrix
          glPopMatrix
          glLoadIdentity
          glLoadMatrixd
          glLoadMatrixf
          glMultMatrixd
          glMultMatrixf
          glRotated
          glRotatef
          glScaled
          glScalef
          glTranslated
          glTranslatef
          glIsList
          glDeleteLists
          glGenLists
          glNewList
          glEndList
          glCallList
          glCallLists
          glListBase
          glBegin
          glEnd
          glVertex2d
          glVertex2f
          glVertex2i
          glVertex2s
          glVertex3d
          glVertex3f
          glVertex3i
          glVertex3s
          glVertex4d
          glVertex4f
          glVertex4i
          glVertex4s
          glVertex2dv
          glVertex2fv
          glVertex2iv
          glVertex2sv
          glVertex3dv
          glVertex3fv
          glVertex3iv
          glVertex3sv
          glVertex4dv
          glVertex4fv
          glVertex4iv
          glVertex4sv
          glNormal3b
          glNormal3d
          glNormal3f
          glNormal3i
          glNormal3s
          glNormal3bv
          glNormal3dv
          glNormal3fv
          glNormal3iv
          glNormal3sv
          glIndexd
          glIndexf
          glIndexi
          glIndexs
          glIndexub
          glIndexdv
          glIndexfv
          glIndexiv
          glIndexsv
          glIndexubv
          glColor3b
          glColor3d
          glColor3f
          glColor3i
          glColor3s
          glColor3ub
          glColor3ui
          glColor3us
          glColor4b
          glColor4d
          glColor4f
          glColor4i
          glColor4s
          glColor4ub
          glColor4ui
          glColor4us
          glColor3bv
          glColor3dv
          glColor3fv
          glColor3iv
          glColor3sv
          glColor3ubv
          glColor3uiv
          glColor3usv
          glColor4bv
          glColor4dv
          glColor4fv
          glColor4iv
          glColor4sv
          glColor4ubv
          glColor4uiv
          glColor4usv
          glTexCoord1d
          glTexCoord1f
          glTexCoord1i
          glTexCoord1s
          glTexCoord2d
          glTexCoord2f
          glTexCoord2i
          glTexCoord2s
          glTexCoord3d
          glTexCoord3f
          glTexCoord3i
          glTexCoord3s
          glTexCoord4d
          glTexCoord4f
          glTexCoord4i
          glTexCoord4s
          glTexCoord1dv
          glTexCoord1fv
          glTexCoord1iv
          glTexCoord1sv
          glTexCoord2dv
          glTexCoord2fv
          glTexCoord2iv
          glTexCoord2sv
          glTexCoord3dv
          glTexCoord3fv
          glTexCoord3iv
          glTexCoord3sv
          glTexCoord4dv
          glTexCoord4fv
          glTexCoord4iv
          glTexCoord4sv
          glRasterPos2d
          glRasterPos2f
          glRasterPos2i
          glRasterPos2s
          glRasterPos3d
          glRasterPos3f
          glRasterPos3i
          glRasterPos3s
          glRasterPos4d
          glRasterPos4f
          glRasterPos4i
          glRasterPos4s
          glRasterPos2dv
          glRasterPos2fv
          glRasterPos2iv
          glRasterPos2sv
          glRasterPos3dv
          glRasterPos3fv
          glRasterPos3iv
          glRasterPos3sv
          glRasterPos4dv
          glRasterPos4fv
          glRasterPos4iv
          glRasterPos4sv
          glRectd
          glRectf
          glRecti
          glRects
          glRectdv
          glRectfv
          glRectiv
          glRectsv
          glVertexPointer
          glNormalPointer
          glColorPointer
          glIndexPointer
          glTexCoordPointer
          glEdgeFlagPointer
          glGetPointerv
          glArrayElement
          glDrawArrays
          glDrawElements
          glInterleavedArrays
          glShadeModel
          glLightf
          glLighti
          glLightfv
          glLightiv
          glGetLightfv
          glGetLightiv
          glLightModelf
          glLightModeli
          glLightModelfv
          glLightModeliv
          glMaterialf
          glMateriali
          glMaterialfv
          glMaterialiv
          glGetMaterialfv
          glGetMaterialiv
          glColorMaterial
          glPixelZoom
          glPixelStoref
          glPixelStorei
          glPixelTransferf
          glPixelTransferi
          glPixelMapfv
          glPixelMapuiv
          glPixelMapusv
          glGetPixelMapfv
          glGetPixelMapuiv
          glGetPixelMapusv
          glBitmap
          glReadPixels
          glDrawPixels
          glCopyPixels
          glStencilFunc
          glStencilMask
          glStencilOp
          glClearStencil
          glTexGend
          glTexGenf
          glTexGeni
          glTexGendv
          glTexGenfv
          glTexGeniv
          glGetTexGendv
          glGetTexGenfv
          glGetTexGeniv
          glTexEnvf
          glTexEnvi
          glTexEnvfv
          glTexEnviv
          glGetTexEnvfv
          glGetTexEnviv
          glTexParameterf
          glTexParameteri
          glTexParameterfv
          glTexParameteriv
          glGetTexParameterfv
          glGetTexParameteriv
          glGetTexLevelParameterfv
          glGetTexLevelParameteriv
          glTexImage1D
          glTexImage2D
          glGetTexImage
          glGenTextures
          glDeleteTextures
          glBindTexture
          glPrioritizeTextures
          glAreTexturesResident
          glIsTexture
          glTexSubImage1D
          glTexSubImage2D
          glCopyTexImage1D
          glCopyTexImage2D
          glCopyTexSubImage1D
          glCopyTexSubImage2D
          glMap1d
          glMap1f
          glMap2d
          glMap2f
          glGetMapdv
          glGetMapfv
          glGetMapiv
          glEvalCoord1d
          glEvalCoord1f
          glEvalCoord1dv
          glEvalCoord1fv
          glEvalCoord2d
          glEvalCoord2f
          glEvalCoord2dv
          glEvalCoord2fv
          glMapGrid1d
          glMapGrid1f
          glMapGrid2d
          glMapGrid2f
          glEvalPoint1
          glEvalPoint2
          glEvalMesh1
          glEvalMesh2
          glFogf
          glFogi
          glFogfv
          glFogiv
          glFeedbackBuffer
          glPassThrough
          glSelectBuffer
          glInitNames
          glLoadName
          glPushName
          glPopName
          glDrawRangeElements
          glTexImage3D
          glTexSubImage3D
          glCopyTexSubImage3D
          glColorTable
          glColorSubTable
          glColorTableParameteriv
          glColorTableParameterfv
          glCopyColorSubTable
          glCopyColorTable
          glGetColorTable
          glGetColorTableParameterfv
          glGetColorTableParameteriv
          glBlendEquation
          glBlendColor
          glHistogram
          glResetHistogram
          glGetHistogram
          glGetHistogramParameterfv
          glGetHistogramParameteriv
          glMinmax
          glResetMinmax
          glGetMinmax
          glGetMinmaxParameterfv
          glGetMinmaxParameteriv
          glConvolutionFilter1D
          glConvolutionFilter2D
          glConvolutionParameterf
          glConvolutionParameterfv
          glConvolutionParameteri
          glConvolutionParameteriv
          glCopyConvolutionFilter1D
          glCopyConvolutionFilter2D
          glGetConvolutionFilter
          glGetConvolutionParameterfv
          glGetConvolutionParameteriv
          glSeparableFilter2D
          glGetSeparableFilter
          glActiveTexture
          glClientActiveTexture
          glCompressedTexImage1D
          glCompressedTexImage2D
          glCompressedTexImage3D
          glCompressedTexSubImage1D
          glCompressedTexSubImage2D
          glCompressedTexSubImage3D
          glGetCompressedTexImage
          glMultiTexCoord1d
          glMultiTexCoord1dv
          glMultiTexCoord1f
          glMultiTexCoord1fv
          glMultiTexCoord1i
          glMultiTexCoord1iv
          glMultiTexCoord1s
          glMultiTexCoord1sv
          glMultiTexCoord2d
          glMultiTexCoord2dv
          glMultiTexCoord2f
          glMultiTexCoord2fv
          glMultiTexCoord2i
          glMultiTexCoord2iv
          glMultiTexCoord2s
          glMultiTexCoord2sv
          glMultiTexCoord3d
          glMultiTexCoord3dv
          glMultiTexCoord3f
          glMultiTexCoord3fv
          glMultiTexCoord3i
          glMultiTexCoord3iv
          glMultiTexCoord3s
          glMultiTexCoord3sv
          glMultiTexCoord4d
          glMultiTexCoord4dv
          glMultiTexCoord4f
          glMultiTexCoord4fv
          glMultiTexCoord4i
          glMultiTexCoord4iv
          glMultiTexCoord4s
          glMultiTexCoord4sv
          glLoadTransposeMatrixd
          glLoadTransposeMatrixf
          glMultTransposeMatrixd
          glMultTransposeMatrixf
          glSampleCoverage
          glActiveTextureARB
          glClientActiveTextureARB
          glMultiTexCoord1dARB
          glMultiTexCoord1dvARB
          glMultiTexCoord1fARB
          glMultiTexCoord1fvARB
          glMultiTexCoord1iARB
          glMultiTexCoord1ivARB
          glMultiTexCoord1sARB
          glMultiTexCoord1svARB
          glMultiTexCoord2dARB
          glMultiTexCoord2dvARB
          glMultiTexCoord2fARB
          glMultiTexCoord2fvARB
          glMultiTexCoord2iARB
          glMultiTexCoord2ivARB
          glMultiTexCoord2sARB
          glMultiTexCoord2svARB
          glMultiTexCoord3dARB
          glMultiTexCoord3dvARB
          glMultiTexCoord3fARB
          glMultiTexCoord3fvARB
          glMultiTexCoord3iARB
          glMultiTexCoord3ivARB
          glMultiTexCoord3sARB
          glMultiTexCoord3svARB
          glMultiTexCoord4dARB
          glMultiTexCoord4dvARB
          glMultiTexCoord4fARB
          glMultiTexCoord4fvARB
          glMultiTexCoord4iARB
          glMultiTexCoord4ivARB
          glMultiTexCoord4sARB
          glMultiTexCoord4svARB)

  (import (rnrs) (ypsilon-compat))

  (define libGL (cond (on-darwin  (load-shared-object "OpenGL.framework/OpenGL"))
                      (on-windows (load-shared-object "opengl32.dll"))
                      (on-linux   (load-shared-object "libGL.so.1"))
                      (on-freebsd (load-shared-object "libGL.so.1"))
                      (else       (assertion-violation #f "can not locate OpenGL library, unknown operating system"))))
  
  ;;;; Boolean values
  (define GL_FALSE #x0)
  (define GL_TRUE #x1)
  ;;;; Data types
  (define GL_BYTE #x1400)
  (define GL_UNSIGNED_BYTE #x1401)
  (define GL_SHORT #x1402)
  (define GL_UNSIGNED_SHORT #x1403)
  (define GL_INT #x1404)
  (define GL_UNSIGNED_INT #x1405)
  (define GL_FLOAT #x1406)
  (define GL_2_BYTES #x1407)
  (define GL_3_BYTES #x1408)
  (define GL_4_BYTES #x1409)
  (define GL_DOUBLE #x140A)
  ;;;; Primitives
  (define GL_POINTS #x0000)
  (define GL_LINES #x0001)
  (define GL_LINE_LOOP #x0002)
  (define GL_LINE_STRIP #x0003)
  (define GL_TRIANGLES #x0004)
  (define GL_TRIANGLE_STRIP #x0005)
  (define GL_TRIANGLE_FAN #x0006)
  (define GL_QUADS #x0007)
  (define GL_QUAD_STRIP #x0008)
  (define GL_POLYGON #x0009)
  ;;;; Vertex Arrays
  (define GL_VERTEX_ARRAY #x8074)
  (define GL_NORMAL_ARRAY #x8075)
  (define GL_COLOR_ARRAY #x8076)
  (define GL_INDEX_ARRAY #x8077)
  (define GL_TEXTURE_COORD_ARRAY #x8078)
  (define GL_EDGE_FLAG_ARRAY #x8079)
  (define GL_VERTEX_ARRAY_SIZE #x807A)
  (define GL_VERTEX_ARRAY_TYPE #x807B)
  (define GL_VERTEX_ARRAY_STRIDE #x807C)
  (define GL_NORMAL_ARRAY_TYPE #x807E)
  (define GL_NORMAL_ARRAY_STRIDE #x807F)
  (define GL_COLOR_ARRAY_SIZE #x8081)
  (define GL_COLOR_ARRAY_TYPE #x8082)
  (define GL_COLOR_ARRAY_STRIDE #x8083)
  (define GL_INDEX_ARRAY_TYPE #x8085)
  (define GL_INDEX_ARRAY_STRIDE #x8086)
  (define GL_TEXTURE_COORD_ARRAY_SIZE #x8088)
  (define GL_TEXTURE_COORD_ARRAY_TYPE #x8089)
  (define GL_TEXTURE_COORD_ARRAY_STRIDE #x808A)
  (define GL_EDGE_FLAG_ARRAY_STRIDE #x808C)
  (define GL_VERTEX_ARRAY_POINTER #x808E)
  (define GL_NORMAL_ARRAY_POINTER #x808F)
  (define GL_COLOR_ARRAY_POINTER #x8090)
  (define GL_INDEX_ARRAY_POINTER #x8091)
  (define GL_TEXTURE_COORD_ARRAY_POINTER #x8092)
  (define GL_EDGE_FLAG_ARRAY_POINTER #x8093)
  (define GL_V2F #x2A20)
  (define GL_V3F #x2A21)
  (define GL_C4UB_V2F #x2A22)
  (define GL_C4UB_V3F #x2A23)
  (define GL_C3F_V3F #x2A24)
  (define GL_N3F_V3F #x2A25)
  (define GL_C4F_N3F_V3F #x2A26)
  (define GL_T2F_V3F #x2A27)
  (define GL_T4F_V4F #x2A28)
  (define GL_T2F_C4UB_V3F #x2A29)
  (define GL_T2F_C3F_V3F #x2A2A)
  (define GL_T2F_N3F_V3F #x2A2B)
  (define GL_T2F_C4F_N3F_V3F #x2A2C)
  (define GL_T4F_C4F_N3F_V4F #x2A2D)
  ;;;; Matrix Mode
  (define GL_MATRIX_MODE #x0BA0)
  (define GL_MODELVIEW #x1700)
  (define GL_PROJECTION #x1701)
  (define GL_TEXTURE #x1702)
  ;;;; Points
  (define GL_POINT_SMOOTH #x0B10)
  (define GL_POINT_SIZE #x0B11)
  (define GL_POINT_SIZE_GRANULARITY #x0B13)
  (define GL_POINT_SIZE_RANGE #x0B12)
  ;;;; Lines
  (define GL_LINE_SMOOTH #x0B20)
  (define GL_LINE_STIPPLE #x0B24)
  (define GL_LINE_STIPPLE_PATTERN #x0B25)
  (define GL_LINE_STIPPLE_REPEAT #x0B26)
  (define GL_LINE_WIDTH #x0B21)
  (define GL_LINE_WIDTH_GRANULARITY #x0B23)
  (define GL_LINE_WIDTH_RANGE #x0B22)
  ;;;; Polygons
  (define GL_POINT #x1B00)
  (define GL_LINE #x1B01)
  (define GL_FILL #x1B02)
  (define GL_CW #x0900)
  (define GL_CCW #x0901)
  (define GL_FRONT #x0404)
  (define GL_BACK #x0405)
  (define GL_POLYGON_MODE #x0B40)
  (define GL_POLYGON_SMOOTH #x0B41)
  (define GL_POLYGON_STIPPLE #x0B42)
  (define GL_EDGE_FLAG #x0B43)
  (define GL_CULL_FACE #x0B44)
  (define GL_CULL_FACE_MODE #x0B45)
  (define GL_FRONT_FACE #x0B46)
  (define GL_POLYGON_OFFSET_FACTOR #x8038)
  (define GL_POLYGON_OFFSET_UNITS #x2A00)
  (define GL_POLYGON_OFFSET_POINT #x2A01)
  (define GL_POLYGON_OFFSET_LINE #x2A02)
  (define GL_POLYGON_OFFSET_FILL #x8037)
  ;;;; Display Lists
  (define GL_COMPILE #x1300)
  (define GL_COMPILE_AND_EXECUTE #x1301)
  (define GL_LIST_BASE #x0B32)
  (define GL_LIST_INDEX #x0B33)
  (define GL_LIST_MODE #x0B30)
  ;;;; Depth buffer
  (define GL_NEVER #x0200)
  (define GL_LESS #x0201)
  (define GL_EQUAL #x0202)
  (define GL_LEQUAL #x0203)
  (define GL_GREATER #x0204)
  (define GL_NOTEQUAL #x0205)
  (define GL_GEQUAL #x0206)
  (define GL_ALWAYS #x0207)
  (define GL_DEPTH_TEST #x0B71)
  (define GL_DEPTH_BITS #x0D56)
  (define GL_DEPTH_CLEAR_VALUE #x0B73)
  (define GL_DEPTH_FUNC #x0B74)
  (define GL_DEPTH_RANGE #x0B70)
  (define GL_DEPTH_WRITEMASK #x0B72)
  (define GL_DEPTH_COMPONENT #x1902)
  ;;;; Lighting
  (define GL_LIGHTING #x0B50)
  (define GL_LIGHT0 #x4000)
  (define GL_LIGHT1 #x4001)
  (define GL_LIGHT2 #x4002)
  (define GL_LIGHT3 #x4003)
  (define GL_LIGHT4 #x4004)
  (define GL_LIGHT5 #x4005)
  (define GL_LIGHT6 #x4006)
  (define GL_LIGHT7 #x4007)
  (define GL_SPOT_EXPONENT #x1205)
  (define GL_SPOT_CUTOFF #x1206)
  (define GL_CONSTANT_ATTENUATION #x1207)
  (define GL_LINEAR_ATTENUATION #x1208)
  (define GL_QUADRATIC_ATTENUATION #x1209)
  (define GL_AMBIENT #x1200)
  (define GL_DIFFUSE #x1201)
  (define GL_SPECULAR #x1202)
  (define GL_SHININESS #x1601)
  (define GL_EMISSION #x1600)
  (define GL_POSITION #x1203)
  (define GL_SPOT_DIRECTION #x1204)
  (define GL_AMBIENT_AND_DIFFUSE #x1602)
  (define GL_COLOR_INDEXES #x1603)
  (define GL_LIGHT_MODEL_TWO_SIDE #x0B52)
  (define GL_LIGHT_MODEL_LOCAL_VIEWER #x0B51)
  (define GL_LIGHT_MODEL_AMBIENT #x0B53)
  (define GL_FRONT_AND_BACK #x0408)
  (define GL_SHADE_MODEL #x0B54)
  (define GL_FLAT #x1D00)
  (define GL_SMOOTH #x1D01)
  (define GL_COLOR_MATERIAL #x0B57)
  (define GL_COLOR_MATERIAL_FACE #x0B55)
  (define GL_COLOR_MATERIAL_PARAMETER #x0B56)
  (define GL_NORMALIZE #x0BA1)
  ;;;; User clipping planes
  (define GL_CLIP_PLANE0 #x3000)
  (define GL_CLIP_PLANE1 #x3001)
  (define GL_CLIP_PLANE2 #x3002)
  (define GL_CLIP_PLANE3 #x3003)
  (define GL_CLIP_PLANE4 #x3004)
  (define GL_CLIP_PLANE5 #x3005)
  ;;;; Accumulation buffer
  (define GL_ACCUM_RED_BITS #x0D58)
  (define GL_ACCUM_GREEN_BITS #x0D59)
  (define GL_ACCUM_BLUE_BITS #x0D5A)
  (define GL_ACCUM_ALPHA_BITS #x0D5B)
  (define GL_ACCUM_CLEAR_VALUE #x0B80)
  (define GL_ACCUM #x0100)
  (define GL_ADD #x0104)
  (define GL_LOAD #x0101)
  (define GL_MULT #x0103)
  (define GL_RETURN #x0102)
  ;;;; Alpha testing
  (define GL_ALPHA_TEST #x0BC0)
  (define GL_ALPHA_TEST_REF #x0BC2)
  (define GL_ALPHA_TEST_FUNC #x0BC1)
  ;;;; Blending
  (define GL_BLEND #x0BE2)
  (define GL_BLEND_SRC #x0BE1)
  (define GL_BLEND_DST #x0BE0)
  (define GL_ZERO #x0)
  (define GL_ONE #x1)
  (define GL_SRC_COLOR #x0300)
  (define GL_ONE_MINUS_SRC_COLOR #x0301)
  (define GL_SRC_ALPHA #x0302)
  (define GL_ONE_MINUS_SRC_ALPHA #x0303)
  (define GL_DST_ALPHA #x0304)
  (define GL_ONE_MINUS_DST_ALPHA #x0305)
  (define GL_DST_COLOR #x0306)
  (define GL_ONE_MINUS_DST_COLOR #x0307)
  (define GL_SRC_ALPHA_SATURATE #x0308)
  ;;;; Render Mode
  (define GL_FEEDBACK #x1C01)
  (define GL_RENDER #x1C00)
  (define GL_SELECT #x1C02)
  ;;;; Feedback
  (define GL_2D #x0600)
  (define GL_3D #x0601)
  (define GL_3D_COLOR #x0602)
  (define GL_3D_COLOR_TEXTURE #x0603)
  (define GL_4D_COLOR_TEXTURE #x0604)
  (define GL_POINT_TOKEN #x0701)
  (define GL_LINE_TOKEN #x0702)
  (define GL_LINE_RESET_TOKEN #x0707)
  (define GL_POLYGON_TOKEN #x0703)
  (define GL_BITMAP_TOKEN #x0704)
  (define GL_DRAW_PIXEL_TOKEN #x0705)
  (define GL_COPY_PIXEL_TOKEN #x0706)
  (define GL_PASS_THROUGH_TOKEN #x0700)
  (define GL_FEEDBACK_BUFFER_POINTER #x0DF0)
  (define GL_FEEDBACK_BUFFER_SIZE #x0DF1)
  (define GL_FEEDBACK_BUFFER_TYPE #x0DF2)
  ;;;; Selection
  (define GL_SELECTION_BUFFER_POINTER #x0DF3)
  (define GL_SELECTION_BUFFER_SIZE #x0DF4)
  ;;;; Fog
  (define GL_FOG #x0B60)
  (define GL_FOG_MODE #x0B65)
  (define GL_FOG_DENSITY #x0B62)
  (define GL_FOG_COLOR #x0B66)
  (define GL_FOG_INDEX #x0B61)
  (define GL_FOG_START #x0B63)
  (define GL_FOG_END #x0B64)
  (define GL_LINEAR #x2601)
  (define GL_EXP #x0800)
  (define GL_EXP2 #x0801)
  ;;;; Logic Ops
  (define GL_LOGIC_OP #x0BF1)
  (define GL_INDEX_LOGIC_OP #x0BF1)
  (define GL_COLOR_LOGIC_OP #x0BF2)
  (define GL_LOGIC_OP_MODE #x0BF0)
  (define GL_CLEAR #x1500)
  (define GL_SET #x150F)
  (define GL_COPY #x1503)
  (define GL_COPY_INVERTED #x150C)
  (define GL_NOOP #x1505)
  (define GL_INVERT #x150A)
  (define GL_AND #x1501)
  (define GL_NAND #x150E)
  (define GL_OR #x1507)
  (define GL_NOR #x1508)
  (define GL_XOR #x1506)
  (define GL_EQUIV #x1509)
  (define GL_AND_REVERSE #x1502)
  (define GL_AND_INVERTED #x1504)
  (define GL_OR_REVERSE #x150B)
  (define GL_OR_INVERTED #x150D)
  ;;;; Stencil
  (define GL_STENCIL_BITS #x0D57)
  (define GL_STENCIL_TEST #x0B90)
  (define GL_STENCIL_CLEAR_VALUE #x0B91)
  (define GL_STENCIL_FUNC #x0B92)
  (define GL_STENCIL_VALUE_MASK #x0B93)
  (define GL_STENCIL_FAIL #x0B94)
  (define GL_STENCIL_PASS_DEPTH_FAIL #x0B95)
  (define GL_STENCIL_PASS_DEPTH_PASS #x0B96)
  (define GL_STENCIL_REF #x0B97)
  (define GL_STENCIL_WRITEMASK #x0B98)
  (define GL_STENCIL_INDEX #x1901)
  (define GL_KEEP #x1E00)
  (define GL_REPLACE #x1E01)
  (define GL_INCR #x1E02)
  (define GL_DECR #x1E03)
  ;;;; Buffers, Pixel Drawing/Reading
  (define GL_NONE #x0)
  (define GL_LEFT #x0406)
  (define GL_RIGHT #x0407)
  ;;;;GL_FRONT					0x0404
  ;;;;GL_BACK					0x0405
  ;;;;GL_FRONT_AND_BACK			0x0408
  (define GL_FRONT_LEFT #x0400)
  (define GL_FRONT_RIGHT #x0401)
  (define GL_BACK_LEFT #x0402)
  (define GL_BACK_RIGHT #x0403)
  (define GL_AUX0 #x0409)
  (define GL_AUX1 #x040A)
  (define GL_AUX2 #x040B)
  (define GL_AUX3 #x040C)
  (define GL_COLOR_INDEX #x1900)
  (define GL_RED #x1903)
  (define GL_GREEN #x1904)
  (define GL_BLUE #x1905)
  (define GL_ALPHA #x1906)
  (define GL_LUMINANCE #x1909)
  (define GL_LUMINANCE_ALPHA #x190A)
  (define GL_ALPHA_BITS #x0D55)
  (define GL_RED_BITS #x0D52)
  (define GL_GREEN_BITS #x0D53)
  (define GL_BLUE_BITS #x0D54)
  (define GL_INDEX_BITS #x0D51)
  (define GL_SUBPIXEL_BITS #x0D50)
  (define GL_AUX_BUFFERS #x0C00)
  (define GL_READ_BUFFER #x0C02)
  (define GL_DRAW_BUFFER #x0C01)
  (define GL_DOUBLEBUFFER #x0C32)
  (define GL_STEREO #x0C33)
  (define GL_BITMAP #x1A00)
  (define GL_COLOR #x1800)
  (define GL_DEPTH #x1801)
  (define GL_STENCIL #x1802)
  (define GL_DITHER #x0BD0)
  (define GL_RGB #x1907)
  (define GL_RGBA #x1908)
  ;;;; Implementation limits
  (define GL_MAX_LIST_NESTING #x0B31)
  (define GL_MAX_EVAL_ORDER #x0D30)
  (define GL_MAX_LIGHTS #x0D31)
  (define GL_MAX_CLIP_PLANES #x0D32)
  (define GL_MAX_TEXTURE_SIZE #x0D33)
  (define GL_MAX_PIXEL_MAP_TABLE #x0D34)
  (define GL_MAX_ATTRIB_STACK_DEPTH #x0D35)
  (define GL_MAX_MODELVIEW_STACK_DEPTH #x0D36)
  (define GL_MAX_NAME_STACK_DEPTH #x0D37)
  (define GL_MAX_PROJECTION_STACK_DEPTH #x0D38)
  (define GL_MAX_TEXTURE_STACK_DEPTH #x0D39)
  (define GL_MAX_VIEWPORT_DIMS #x0D3A)
  (define GL_MAX_CLIENT_ATTRIB_STACK_DEPTH #x0D3B)
  ;;;; Gets
  (define GL_ATTRIB_STACK_DEPTH #x0BB0)
  (define GL_CLIENT_ATTRIB_STACK_DEPTH #x0BB1)
  (define GL_COLOR_CLEAR_VALUE #x0C22)
  (define GL_COLOR_WRITEMASK #x0C23)
  (define GL_CURRENT_INDEX #x0B01)
  (define GL_CURRENT_COLOR #x0B00)
  (define GL_CURRENT_NORMAL #x0B02)
  (define GL_CURRENT_RASTER_COLOR #x0B04)
  (define GL_CURRENT_RASTER_DISTANCE #x0B09)
  (define GL_CURRENT_RASTER_INDEX #x0B05)
  (define GL_CURRENT_RASTER_POSITION #x0B07)
  (define GL_CURRENT_RASTER_TEXTURE_COORDS #x0B06)
  (define GL_CURRENT_RASTER_POSITION_VALID #x0B08)
  (define GL_CURRENT_TEXTURE_COORDS #x0B03)
  (define GL_INDEX_CLEAR_VALUE #x0C20)
  (define GL_INDEX_MODE #x0C30)
  (define GL_INDEX_WRITEMASK #x0C21)
  (define GL_MODELVIEW_MATRIX #x0BA6)
  (define GL_MODELVIEW_STACK_DEPTH #x0BA3)
  (define GL_NAME_STACK_DEPTH #x0D70)
  (define GL_PROJECTION_MATRIX #x0BA7)
  (define GL_PROJECTION_STACK_DEPTH #x0BA4)
  (define GL_RENDER_MODE #x0C40)
  (define GL_RGBA_MODE #x0C31)
  (define GL_TEXTURE_MATRIX #x0BA8)
  (define GL_TEXTURE_STACK_DEPTH #x0BA5)
  (define GL_VIEWPORT #x0BA2)
  ;;;; Evaluators
  (define GL_AUTO_NORMAL #x0D80)
  (define GL_MAP1_COLOR_4 #x0D90)
  (define GL_MAP1_INDEX #x0D91)
  (define GL_MAP1_NORMAL #x0D92)
  (define GL_MAP1_TEXTURE_COORD_1 #x0D93)
  (define GL_MAP1_TEXTURE_COORD_2 #x0D94)
  (define GL_MAP1_TEXTURE_COORD_3 #x0D95)
  (define GL_MAP1_TEXTURE_COORD_4 #x0D96)
  (define GL_MAP1_VERTEX_3 #x0D97)
  (define GL_MAP1_VERTEX_4 #x0D98)
  (define GL_MAP2_COLOR_4 #x0DB0)
  (define GL_MAP2_INDEX #x0DB1)
  (define GL_MAP2_NORMAL #x0DB2)
  (define GL_MAP2_TEXTURE_COORD_1 #x0DB3)
  (define GL_MAP2_TEXTURE_COORD_2 #x0DB4)
  (define GL_MAP2_TEXTURE_COORD_3 #x0DB5)
  (define GL_MAP2_TEXTURE_COORD_4 #x0DB6)
  (define GL_MAP2_VERTEX_3 #x0DB7)
  (define GL_MAP2_VERTEX_4 #x0DB8)
  (define GL_MAP1_GRID_DOMAIN #x0DD0)
  (define GL_MAP1_GRID_SEGMENTS #x0DD1)
  (define GL_MAP2_GRID_DOMAIN #x0DD2)
  (define GL_MAP2_GRID_SEGMENTS #x0DD3)
  (define GL_COEFF #x0A00)
  (define GL_ORDER #x0A01)
  (define GL_DOMAIN #x0A02)
  ;;;; Hints
  (define GL_PERSPECTIVE_CORRECTION_HINT #x0C50)
  (define GL_POINT_SMOOTH_HINT #x0C51)
  (define GL_LINE_SMOOTH_HINT #x0C52)
  (define GL_POLYGON_SMOOTH_HINT #x0C53)
  (define GL_FOG_HINT #x0C54)
  (define GL_DONT_CARE #x1100)
  (define GL_FASTEST #x1101)
  (define GL_NICEST #x1102)
  ;;;; Scissor box
  (define GL_SCISSOR_BOX #x0C10)
  (define GL_SCISSOR_TEST #x0C11)
  ;;;; Pixel Mode / Transfer
  (define GL_MAP_COLOR #x0D10)
  (define GL_MAP_STENCIL #x0D11)
  (define GL_INDEX_SHIFT #x0D12)
  (define GL_INDEX_OFFSET #x0D13)
  (define GL_RED_SCALE #x0D14)
  (define GL_RED_BIAS #x0D15)
  (define GL_GREEN_SCALE #x0D18)
  (define GL_GREEN_BIAS #x0D19)
  (define GL_BLUE_SCALE #x0D1A)
  (define GL_BLUE_BIAS #x0D1B)
  (define GL_ALPHA_SCALE #x0D1C)
  (define GL_ALPHA_BIAS #x0D1D)
  (define GL_DEPTH_SCALE #x0D1E)
  (define GL_DEPTH_BIAS #x0D1F)
  (define GL_PIXEL_MAP_S_TO_S_SIZE #x0CB1)
  (define GL_PIXEL_MAP_I_TO_I_SIZE #x0CB0)
  (define GL_PIXEL_MAP_I_TO_R_SIZE #x0CB2)
  (define GL_PIXEL_MAP_I_TO_G_SIZE #x0CB3)
  (define GL_PIXEL_MAP_I_TO_B_SIZE #x0CB4)
  (define GL_PIXEL_MAP_I_TO_A_SIZE #x0CB5)
  (define GL_PIXEL_MAP_R_TO_R_SIZE #x0CB6)
  (define GL_PIXEL_MAP_G_TO_G_SIZE #x0CB7)
  (define GL_PIXEL_MAP_B_TO_B_SIZE #x0CB8)
  (define GL_PIXEL_MAP_A_TO_A_SIZE #x0CB9)
  (define GL_PIXEL_MAP_S_TO_S #x0C71)
  (define GL_PIXEL_MAP_I_TO_I #x0C70)
  (define GL_PIXEL_MAP_I_TO_R #x0C72)
  (define GL_PIXEL_MAP_I_TO_G #x0C73)
  (define GL_PIXEL_MAP_I_TO_B #x0C74)
  (define GL_PIXEL_MAP_I_TO_A #x0C75)
  (define GL_PIXEL_MAP_R_TO_R #x0C76)
  (define GL_PIXEL_MAP_G_TO_G #x0C77)
  (define GL_PIXEL_MAP_B_TO_B #x0C78)
  (define GL_PIXEL_MAP_A_TO_A #x0C79)
  (define GL_PACK_ALIGNMENT #x0D05)
  (define GL_PACK_LSB_FIRST #x0D01)
  (define GL_PACK_ROW_LENGTH #x0D02)
  (define GL_PACK_SKIP_PIXELS #x0D04)
  (define GL_PACK_SKIP_ROWS #x0D03)
  (define GL_PACK_SWAP_BYTES #x0D00)
  (define GL_UNPACK_ALIGNMENT #x0CF5)
  (define GL_UNPACK_LSB_FIRST #x0CF1)
  (define GL_UNPACK_ROW_LENGTH #x0CF2)
  (define GL_UNPACK_SKIP_PIXELS #x0CF4)
  (define GL_UNPACK_SKIP_ROWS #x0CF3)
  (define GL_UNPACK_SWAP_BYTES #x0CF0)
  (define GL_ZOOM_X #x0D16)
  (define GL_ZOOM_Y #x0D17)
  ;;;; Texture mapping
  (define GL_TEXTURE_ENV #x2300)
  (define GL_TEXTURE_ENV_MODE #x2200)
  (define GL_TEXTURE_1D #x0DE0)
  (define GL_TEXTURE_2D #x0DE1)
  (define GL_TEXTURE_WRAP_S #x2802)
  (define GL_TEXTURE_WRAP_T #x2803)
  (define GL_TEXTURE_MAG_FILTER #x2800)
  (define GL_TEXTURE_MIN_FILTER #x2801)
  (define GL_TEXTURE_ENV_COLOR #x2201)
  (define GL_TEXTURE_GEN_S #x0C60)
  (define GL_TEXTURE_GEN_T #x0C61)
  (define GL_TEXTURE_GEN_MODE #x2500)
  (define GL_TEXTURE_BORDER_COLOR #x1004)
  (define GL_TEXTURE_WIDTH #x1000)
  (define GL_TEXTURE_HEIGHT #x1001)
  (define GL_TEXTURE_BORDER #x1005)
  (define GL_TEXTURE_COMPONENTS #x1003)
  (define GL_TEXTURE_RED_SIZE #x805C)
  (define GL_TEXTURE_GREEN_SIZE #x805D)
  (define GL_TEXTURE_BLUE_SIZE #x805E)
  (define GL_TEXTURE_ALPHA_SIZE #x805F)
  (define GL_TEXTURE_LUMINANCE_SIZE #x8060)
  (define GL_TEXTURE_INTENSITY_SIZE #x8061)
  (define GL_NEAREST_MIPMAP_NEAREST #x2700)
  (define GL_NEAREST_MIPMAP_LINEAR #x2702)
  (define GL_LINEAR_MIPMAP_NEAREST #x2701)
  (define GL_LINEAR_MIPMAP_LINEAR #x2703)
  (define GL_OBJECT_LINEAR #x2401)
  (define GL_OBJECT_PLANE #x2501)
  (define GL_EYE_LINEAR #x2400)
  (define GL_EYE_PLANE #x2502)
  (define GL_SPHERE_MAP #x2402)
  (define GL_DECAL #x2101)
  (define GL_MODULATE #x2100)
  (define GL_NEAREST #x2600)
  (define GL_REPEAT #x2901)
  (define GL_CLAMP #x2900)
  (define GL_S #x2000)
  (define GL_T #x2001)
  (define GL_R #x2002)
  (define GL_Q #x2003)
  (define GL_TEXTURE_GEN_R #x0C62)
  (define GL_TEXTURE_GEN_Q #x0C63)
  ;;;; Utility
  (define GL_VENDOR #x1F00)
  (define GL_RENDERER #x1F01)
  (define GL_VERSION #x1F02)
  (define GL_EXTENSIONS #x1F03)
  ;;;; Errors
  (define GL_NO_ERROR #x0)
  (define GL_INVALID_ENUM #x0500)
  (define GL_INVALID_VALUE #x0501)
  (define GL_INVALID_OPERATION #x0502)
  (define GL_STACK_OVERFLOW #x0503)
  (define GL_STACK_UNDERFLOW #x0504)
  (define GL_OUT_OF_MEMORY #x0505)
  ;;;; glPush/PopAttrib bits
  (define GL_CURRENT_BIT #x00000001)
  (define GL_POINT_BIT #x00000002)
  (define GL_LINE_BIT #x00000004)
  (define GL_POLYGON_BIT #x00000008)
  (define GL_POLYGON_STIPPLE_BIT #x00000010)
  (define GL_PIXEL_MODE_BIT #x00000020)
  (define GL_LIGHTING_BIT #x00000040)
  (define GL_FOG_BIT #x00000080)
  (define GL_DEPTH_BUFFER_BIT #x00000100)
  (define GL_ACCUM_BUFFER_BIT #x00000200)
  (define GL_STENCIL_BUFFER_BIT #x00000400)
  (define GL_VIEWPORT_BIT #x00000800)
  (define GL_TRANSFORM_BIT #x00001000)
  (define GL_ENABLE_BIT #x00002000)
  (define GL_COLOR_BUFFER_BIT #x00004000)
  (define GL_HINT_BIT #x00008000)
  (define GL_EVAL_BIT #x00010000)
  (define GL_LIST_BIT #x00020000)
  (define GL_TEXTURE_BIT #x00040000)
  (define GL_SCISSOR_BIT #x00080000)
  (define GL_ALL_ATTRIB_BITS #x000FFFFF)
  ;;;; OpenGL 1.1
  (define GL_PROXY_TEXTURE_1D #x8063)
  (define GL_PROXY_TEXTURE_2D #x8064)
  (define GL_TEXTURE_PRIORITY #x8066)
  (define GL_TEXTURE_RESIDENT #x8067)
  (define GL_TEXTURE_BINDING_1D #x8068)
  (define GL_TEXTURE_BINDING_2D #x8069)
  (define GL_TEXTURE_INTERNAL_FORMAT #x1003)
  (define GL_ALPHA4 #x803B)
  (define GL_ALPHA8 #x803C)
  (define GL_ALPHA12 #x803D)
  (define GL_ALPHA16 #x803E)
  (define GL_LUMINANCE4 #x803F)
  (define GL_LUMINANCE8 #x8040)
  (define GL_LUMINANCE12 #x8041)
  (define GL_LUMINANCE16 #x8042)
  (define GL_LUMINANCE4_ALPHA4 #x8043)
  (define GL_LUMINANCE6_ALPHA2 #x8044)
  (define GL_LUMINANCE8_ALPHA8 #x8045)
  (define GL_LUMINANCE12_ALPHA4 #x8046)
  (define GL_LUMINANCE12_ALPHA12 #x8047)
  (define GL_LUMINANCE16_ALPHA16 #x8048)
  (define GL_INTENSITY #x8049)
  (define GL_INTENSITY4 #x804A)
  (define GL_INTENSITY8 #x804B)
  (define GL_INTENSITY12 #x804C)
  (define GL_INTENSITY16 #x804D)
  (define GL_R3_G3_B2 #x2A10)
  (define GL_RGB4 #x804F)
  (define GL_RGB5 #x8050)
  (define GL_RGB8 #x8051)
  (define GL_RGB10 #x8052)
  (define GL_RGB12 #x8053)
  (define GL_RGB16 #x8054)
  (define GL_RGBA2 #x8055)
  (define GL_RGBA4 #x8056)
  (define GL_RGB5_A1 #x8057)
  (define GL_RGBA8 #x8058)
  (define GL_RGB10_A2 #x8059)
  (define GL_RGBA12 #x805A)
  (define GL_RGBA16 #x805B)
  (define GL_CLIENT_PIXEL_STORE_BIT #x00000001)
  (define GL_CLIENT_VERTEX_ARRAY_BIT #x00000002)
  (define GL_ALL_CLIENT_ATTRIB_BITS #xFFFFFFFF)
  (define GL_CLIENT_ALL_ATTRIB_BITS #xFFFFFFFF)
  ;;;; OpenGL 1.2
  (define GL_RESCALE_NORMAL #x803A)
  (define GL_CLAMP_TO_EDGE #x812F)
  (define GL_MAX_ELEMENTS_VERTICES #x80E8)
  (define GL_MAX_ELEMENTS_INDICES #x80E9)
  (define GL_BGR #x80E0)
  (define GL_BGRA #x80E1)
  (define GL_UNSIGNED_BYTE_3_3_2 #x8032)
  (define GL_UNSIGNED_BYTE_2_3_3_REV #x8362)
  (define GL_UNSIGNED_SHORT_5_6_5 #x8363)
  (define GL_UNSIGNED_SHORT_5_6_5_REV #x8364)
  (define GL_UNSIGNED_SHORT_4_4_4_4 #x8033)
  (define GL_UNSIGNED_SHORT_4_4_4_4_REV #x8365)
  (define GL_UNSIGNED_SHORT_5_5_5_1 #x8034)
  (define GL_UNSIGNED_SHORT_1_5_5_5_REV #x8366)
  (define GL_UNSIGNED_INT_8_8_8_8 #x8035)
  (define GL_UNSIGNED_INT_8_8_8_8_REV #x8367)
  (define GL_UNSIGNED_INT_10_10_10_2 #x8036)
  (define GL_UNSIGNED_INT_2_10_10_10_REV #x8368)
  (define GL_LIGHT_MODEL_COLOR_CONTROL #x81F8)
  (define GL_SINGLE_COLOR #x81F9)
  (define GL_SEPARATE_SPECULAR_COLOR #x81FA)
  (define GL_TEXTURE_MIN_LOD #x813A)
  (define GL_TEXTURE_MAX_LOD #x813B)
  (define GL_TEXTURE_BASE_LEVEL #x813C)
  (define GL_TEXTURE_MAX_LEVEL #x813D)
  (define GL_SMOOTH_POINT_SIZE_RANGE #x0B12)
  (define GL_SMOOTH_POINT_SIZE_GRANULARITY #x0B13)
  (define GL_SMOOTH_LINE_WIDTH_RANGE #x0B22)
  (define GL_SMOOTH_LINE_WIDTH_GRANULARITY #x0B23)
  (define GL_ALIASED_POINT_SIZE_RANGE #x846D)
  (define GL_ALIASED_LINE_WIDTH_RANGE #x846E)
  (define GL_PACK_SKIP_IMAGES #x806B)
  (define GL_PACK_IMAGE_HEIGHT #x806C)
  (define GL_UNPACK_SKIP_IMAGES #x806D)
  (define GL_UNPACK_IMAGE_HEIGHT #x806E)
  (define GL_TEXTURE_3D #x806F)
  (define GL_PROXY_TEXTURE_3D #x8070)
  (define GL_TEXTURE_DEPTH #x8071)
  (define GL_TEXTURE_WRAP_R #x8072)
  (define GL_MAX_3D_TEXTURE_SIZE #x8073)
  (define GL_TEXTURE_BINDING_3D #x806A)
  ;;;; GL_ARB_imaging
  (define GL_CONSTANT_COLOR #x8001)
  (define GL_ONE_MINUS_CONSTANT_COLOR #x8002)
  (define GL_CONSTANT_ALPHA #x8003)
  (define GL_ONE_MINUS_CONSTANT_ALPHA #x8004)
  (define GL_COLOR_TABLE #x80D0)
  (define GL_POST_CONVOLUTION_COLOR_TABLE #x80D1)
  (define GL_POST_COLOR_MATRIX_COLOR_TABLE #x80D2)
  (define GL_PROXY_COLOR_TABLE #x80D3)
  (define GL_PROXY_POST_CONVOLUTION_COLOR_TABLE #x80D4)
  (define GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE #x80D5)
  (define GL_COLOR_TABLE_SCALE #x80D6)
  (define GL_COLOR_TABLE_BIAS #x80D7)
  (define GL_COLOR_TABLE_FORMAT #x80D8)
  (define GL_COLOR_TABLE_WIDTH #x80D9)
  (define GL_COLOR_TABLE_RED_SIZE #x80DA)
  (define GL_COLOR_TABLE_GREEN_SIZE #x80DB)
  (define GL_COLOR_TABLE_BLUE_SIZE #x80DC)
  (define GL_COLOR_TABLE_ALPHA_SIZE #x80DD)
  (define GL_COLOR_TABLE_LUMINANCE_SIZE #x80DE)
  (define GL_COLOR_TABLE_INTENSITY_SIZE #x80DF)
  (define GL_CONVOLUTION_1D #x8010)
  (define GL_CONVOLUTION_2D #x8011)
  (define GL_SEPARABLE_2D #x8012)
  (define GL_CONVOLUTION_BORDER_MODE #x8013)
  (define GL_CONVOLUTION_FILTER_SCALE #x8014)
  (define GL_CONVOLUTION_FILTER_BIAS #x8015)
  (define GL_REDUCE #x8016)
  (define GL_CONVOLUTION_FORMAT #x8017)
  (define GL_CONVOLUTION_WIDTH #x8018)
  (define GL_CONVOLUTION_HEIGHT #x8019)
  (define GL_MAX_CONVOLUTION_WIDTH #x801A)
  (define GL_MAX_CONVOLUTION_HEIGHT #x801B)
  (define GL_POST_CONVOLUTION_RED_SCALE #x801C)
  (define GL_POST_CONVOLUTION_GREEN_SCALE #x801D)
  (define GL_POST_CONVOLUTION_BLUE_SCALE #x801E)
  (define GL_POST_CONVOLUTION_ALPHA_SCALE #x801F)
  (define GL_POST_CONVOLUTION_RED_BIAS #x8020)
  (define GL_POST_CONVOLUTION_GREEN_BIAS #x8021)
  (define GL_POST_CONVOLUTION_BLUE_BIAS #x8022)
  (define GL_POST_CONVOLUTION_ALPHA_BIAS #x8023)
  (define GL_CONSTANT_BORDER #x8151)
  (define GL_REPLICATE_BORDER #x8153)
  (define GL_CONVOLUTION_BORDER_COLOR #x8154)
  (define GL_COLOR_MATRIX #x80B1)
  (define GL_COLOR_MATRIX_STACK_DEPTH #x80B2)
  (define GL_MAX_COLOR_MATRIX_STACK_DEPTH #x80B3)
  (define GL_POST_COLOR_MATRIX_RED_SCALE #x80B4)
  (define GL_POST_COLOR_MATRIX_GREEN_SCALE #x80B5)
  (define GL_POST_COLOR_MATRIX_BLUE_SCALE #x80B6)
  (define GL_POST_COLOR_MATRIX_ALPHA_SCALE #x80B7)
  (define GL_POST_COLOR_MATRIX_RED_BIAS #x80B8)
  (define GL_POST_COLOR_MATRIX_GREEN_BIAS #x80B9)
  (define GL_POST_COLOR_MATRIX_BLUE_BIAS #x80BA)
  (define GL_POST_COLOR_MATRIX_ALPHA_BIAS #x80BB)
  (define GL_HISTOGRAM #x8024)
  (define GL_PROXY_HISTOGRAM #x8025)
  (define GL_HISTOGRAM_WIDTH #x8026)
  (define GL_HISTOGRAM_FORMAT #x8027)
  (define GL_HISTOGRAM_RED_SIZE #x8028)
  (define GL_HISTOGRAM_GREEN_SIZE #x8029)
  (define GL_HISTOGRAM_BLUE_SIZE #x802A)
  (define GL_HISTOGRAM_ALPHA_SIZE #x802B)
  (define GL_HISTOGRAM_LUMINANCE_SIZE #x802C)
  (define GL_HISTOGRAM_SINK #x802D)
  (define GL_MINMAX #x802E)
  (define GL_MINMAX_FORMAT #x802F)
  (define GL_MINMAX_SINK #x8030)
  (define GL_TABLE_TOO_LARGE #x8031)
  (define GL_BLEND_EQUATION #x8009)
  (define GL_MIN #x8007)
  (define GL_MAX #x8008)
  (define GL_FUNC_ADD #x8006)
  (define GL_FUNC_SUBTRACT #x800A)
  (define GL_FUNC_REVERSE_SUBTRACT #x800B)
  (define GL_BLEND_COLOR #x8005)
  ;;;; OpenGL 1.3
  ;;;; multitexture
  (define GL_TEXTURE0 #x84C0)
  (define GL_TEXTURE1 #x84C1)
  (define GL_TEXTURE2 #x84C2)
  (define GL_TEXTURE3 #x84C3)
  (define GL_TEXTURE4 #x84C4)
  (define GL_TEXTURE5 #x84C5)
  (define GL_TEXTURE6 #x84C6)
  (define GL_TEXTURE7 #x84C7)
  (define GL_TEXTURE8 #x84C8)
  (define GL_TEXTURE9 #x84C9)
  (define GL_TEXTURE10 #x84CA)
  (define GL_TEXTURE11 #x84CB)
  (define GL_TEXTURE12 #x84CC)
  (define GL_TEXTURE13 #x84CD)
  (define GL_TEXTURE14 #x84CE)
  (define GL_TEXTURE15 #x84CF)
  (define GL_TEXTURE16 #x84D0)
  (define GL_TEXTURE17 #x84D1)
  (define GL_TEXTURE18 #x84D2)
  (define GL_TEXTURE19 #x84D3)
  (define GL_TEXTURE20 #x84D4)
  (define GL_TEXTURE21 #x84D5)
  (define GL_TEXTURE22 #x84D6)
  (define GL_TEXTURE23 #x84D7)
  (define GL_TEXTURE24 #x84D8)
  (define GL_TEXTURE25 #x84D9)
  (define GL_TEXTURE26 #x84DA)
  (define GL_TEXTURE27 #x84DB)
  (define GL_TEXTURE28 #x84DC)
  (define GL_TEXTURE29 #x84DD)
  (define GL_TEXTURE30 #x84DE)
  (define GL_TEXTURE31 #x84DF)
  (define GL_ACTIVE_TEXTURE #x84E0)
  (define GL_CLIENT_ACTIVE_TEXTURE #x84E1)
  (define GL_MAX_TEXTURE_UNITS #x84E2)
  ;;;; texture_cube_map
  (define GL_NORMAL_MAP #x8511)
  (define GL_REFLECTION_MAP #x8512)
  (define GL_TEXTURE_CUBE_MAP #x8513)
  (define GL_TEXTURE_BINDING_CUBE_MAP #x8514)
  (define GL_TEXTURE_CUBE_MAP_POSITIVE_X #x8515)
  (define GL_TEXTURE_CUBE_MAP_NEGATIVE_X #x8516)
  (define GL_TEXTURE_CUBE_MAP_POSITIVE_Y #x8517)
  (define GL_TEXTURE_CUBE_MAP_NEGATIVE_Y #x8518)
  (define GL_TEXTURE_CUBE_MAP_POSITIVE_Z #x8519)
  (define GL_TEXTURE_CUBE_MAP_NEGATIVE_Z #x851A)
  (define GL_PROXY_TEXTURE_CUBE_MAP #x851B)
  (define GL_MAX_CUBE_MAP_TEXTURE_SIZE #x851C)
  ;;;; texture_compression
  (define GL_COMPRESSED_ALPHA #x84E9)
  (define GL_COMPRESSED_LUMINANCE #x84EA)
  (define GL_COMPRESSED_LUMINANCE_ALPHA #x84EB)
  (define GL_COMPRESSED_INTENSITY #x84EC)
  (define GL_COMPRESSED_RGB #x84ED)
  (define GL_COMPRESSED_RGBA #x84EE)
  (define GL_TEXTURE_COMPRESSION_HINT #x84EF)
  (define GL_TEXTURE_COMPRESSED_IMAGE_SIZE #x86A0)
  (define GL_TEXTURE_COMPRESSED #x86A1)
  (define GL_NUM_COMPRESSED_TEXTURE_FORMATS #x86A2)
  (define GL_COMPRESSED_TEXTURE_FORMATS #x86A3)
  ;;;; multisample
  (define GL_MULTISAMPLE #x809D)
  (define GL_SAMPLE_ALPHA_TO_COVERAGE #x809E)
  (define GL_SAMPLE_ALPHA_TO_ONE #x809F)
  (define GL_SAMPLE_COVERAGE #x80A0)
  (define GL_SAMPLE_BUFFERS #x80A8)
  (define GL_SAMPLES #x80A9)
  (define GL_SAMPLE_COVERAGE_VALUE #x80AA)
  (define GL_SAMPLE_COVERAGE_INVERT #x80AB)
  (define GL_MULTISAMPLE_BIT #x20000000)
  ;;;; transpose_matrix
  (define GL_TRANSPOSE_MODELVIEW_MATRIX #x84E3)
  (define GL_TRANSPOSE_PROJECTION_MATRIX #x84E4)
  (define GL_TRANSPOSE_TEXTURE_MATRIX #x84E5)
  (define GL_TRANSPOSE_COLOR_MATRIX #x84E6)
  ;;;; texture_env_combine
  (define GL_COMBINE #x8570)
  (define GL_COMBINE_RGB #x8571)
  (define GL_COMBINE_ALPHA #x8572)
  (define GL_SOURCE0_RGB #x8580)
  (define GL_SOURCE1_RGB #x8581)
  (define GL_SOURCE2_RGB #x8582)
  (define GL_SOURCE0_ALPHA #x8588)
  (define GL_SOURCE1_ALPHA #x8589)
  (define GL_SOURCE2_ALPHA #x858A)
  (define GL_OPERAND0_RGB #x8590)
  (define GL_OPERAND1_RGB #x8591)
  (define GL_OPERAND2_RGB #x8592)
  (define GL_OPERAND0_ALPHA #x8598)
  (define GL_OPERAND1_ALPHA #x8599)
  (define GL_OPERAND2_ALPHA #x859A)
  (define GL_RGB_SCALE #x8573)
  (define GL_ADD_SIGNED #x8574)
  (define GL_INTERPOLATE #x8575)
  (define GL_SUBTRACT #x84E7)
  (define GL_CONSTANT #x8576)
  (define GL_PRIMARY_COLOR #x8577)
  (define GL_PREVIOUS #x8578)
  ;;;; texture_env_dot3
  (define GL_DOT3_RGB #x86AE)
  (define GL_DOT3_RGBA #x86AF)
  ;;;; texture_border_clamp
  (define GL_CLAMP_TO_BORDER #x812D)
  ;;;; GL_ARB_multitexture: ARB extension 1 and OpenGL 1.2.1
  (define GL_TEXTURE0_ARB #x84C0)
  (define GL_TEXTURE1_ARB #x84C1)
  (define GL_TEXTURE2_ARB #x84C2)
  (define GL_TEXTURE3_ARB #x84C3)
  (define GL_TEXTURE4_ARB #x84C4)
  (define GL_TEXTURE5_ARB #x84C5)
  (define GL_TEXTURE6_ARB #x84C6)
  (define GL_TEXTURE7_ARB #x84C7)
  (define GL_TEXTURE8_ARB #x84C8)
  (define GL_TEXTURE9_ARB #x84C9)
  (define GL_TEXTURE10_ARB #x84CA)
  (define GL_TEXTURE11_ARB #x84CB)
  (define GL_TEXTURE12_ARB #x84CC)
  (define GL_TEXTURE13_ARB #x84CD)
  (define GL_TEXTURE14_ARB #x84CE)
  (define GL_TEXTURE15_ARB #x84CF)
  (define GL_TEXTURE16_ARB #x84D0)
  (define GL_TEXTURE17_ARB #x84D1)
  (define GL_TEXTURE18_ARB #x84D2)
  (define GL_TEXTURE19_ARB #x84D3)
  (define GL_TEXTURE20_ARB #x84D4)
  (define GL_TEXTURE21_ARB #x84D5)
  (define GL_TEXTURE22_ARB #x84D6)
  (define GL_TEXTURE23_ARB #x84D7)
  (define GL_TEXTURE24_ARB #x84D8)
  (define GL_TEXTURE25_ARB #x84D9)
  (define GL_TEXTURE26_ARB #x84DA)
  (define GL_TEXTURE27_ARB #x84DB)
  (define GL_TEXTURE28_ARB #x84DC)
  (define GL_TEXTURE29_ARB #x84DD)
  (define GL_TEXTURE30_ARB #x84DE)
  (define GL_TEXTURE31_ARB #x84DF)
  (define GL_ACTIVE_TEXTURE_ARB #x84E0)
  (define GL_CLIENT_ACTIVE_TEXTURE_ARB #x84E1)
  (define GL_MAX_TEXTURE_UNITS_ARB #x84E2)

  (define-syntax define-function
    (syntax-rules ()
      ((_ ret name args)
       (define name (c-function libGL "OpenGL library" ret __stdcall name args)))))

  ;; void glClearIndex( GLfloat c )
  (define-function void glClearIndex (float))

  ;; void glClearColor( GLclampf red, GLclampf green, GLclampf blue, GLclampf alpha )
  (define-function void glClearColor (float float float float))

  ;; void glClear( GLbitfield mask )
  (define-function void glClear (int))

  ;; void glIndexMask( GLuint mask )
  (define-function void glIndexMask (int))

  ;; void glColorMask( GLboolean red, GLboolean green, GLboolean blue, GLboolean alpha )
  (define-function void glColorMask (int int int int))

  ;; void glAlphaFunc( GLenum func, GLclampf ref )
  (define-function void glAlphaFunc (int float))

  ;; void glBlendFunc( GLenum sfactor, GLenum dfactor )
  (define-function void glBlendFunc (int int))

  ;; void glLogicOp( GLenum opcode )
  (define-function void glLogicOp (int))

  ;; void glCullFace( GLenum mode )
  (define-function void glCullFace (int))

  ;; void glFrontFace( GLenum mode )
  (define-function void glFrontFace (int))

  ;; void glPointSize( GLfloat size )
  (define-function void glPointSize (float))

  ;; void glLineWidth( GLfloat width )
  (define-function void glLineWidth (float))

  ;; void glLineStipple( GLint factor, GLushort pattern )
  (define-function void glLineStipple (int int))

  ;; void glPolygonMode( GLenum face, GLenum mode )
  (define-function void glPolygonMode (int int))

  ;; void glPolygonOffset( GLfloat factor, GLfloat units )
  (define-function void glPolygonOffset (float float))

  ;; void glPolygonStipple( const GLubyte *mask )
  (define-function void glPolygonStipple (byte*))

  ;; void glGetPolygonStipple( GLubyte *mask )
  (define-function void glGetPolygonStipple (byte*))

  ;; void glEdgeFlag( GLboolean flag )
  (define-function void glEdgeFlag (int))

  ;; void glEdgeFlagv( const GLboolean *flag )
  (define-function void glEdgeFlagv (byte*))

  ;; void glScissor( GLint x, GLint y, GLsizei width, GLsizei height)
  (define-function void glScissor (int int int int))

  ;; void glClipPlane( GLenum plane, const GLdouble *equation )
  (define-function void glClipPlane (int byte*))

  ;; void glGetClipPlane( GLenum plane, GLdouble *equation )
  (define-function void glGetClipPlane (int byte*))

  ;; void glDrawBuffer( GLenum mode )
  (define-function void glDrawBuffer (int))

  ;; void glReadBuffer( GLenum mode )
  (define-function void glReadBuffer (int))

  ;; void glEnable( GLenum cap )
  (define-function void glEnable (int))

  ;; void glDisable( GLenum cap )
  (define-function void glDisable (int))

  ;; GLboolean glIsEnabled( GLenum cap )
  (define-function int glIsEnabled (int))

  ;; void glEnableClientState( GLenum cap )  /* 1.1 */
  (define-function void glEnableClientState (int))

  ;; void glDisableClientState( GLenum cap )  /* 1.1 */
  (define-function void glDisableClientState (int))

  ;; void glGetBooleanv( GLenum pname, GLboolean *params )
  (define-function void glGetBooleanv (int byte*))

  ;; void glGetDoublev( GLenum pname, GLdouble *params )
  (define-function void glGetDoublev (int byte*))

  ;; void glGetFloatv( GLenum pname, GLfloat *params )
  (define-function void glGetFloatv (int byte*))

  ;; void glGetIntegerv( GLenum pname, GLint *params )
  (define-function void glGetIntegerv (int byte*))

  ;; void glPushAttrib( GLbitfield mask )
  (define-function void glPushAttrib (int))

  ;; void glPopAttrib( void )
  (define-function void glPopAttrib ())

  ;; void glPushClientAttrib( GLbitfield mask )  /* 1.1 */
  (define-function void glPushClientAttrib (int))

  ;; void glPopClientAttrib( void )  /* 1.1 */
  (define-function void glPopClientAttrib ())

  ;; GLint glRenderMode( GLenum mode )
  (define-function int glRenderMode (int))

  ;; GLenum glGetError( void )
  (define-function int glGetError ())

  ;; const GLubyte * glGetString( GLenum name )
  (define-function char* glGetString (int))

  ;; void glFinish( void )
  (define-function void glFinish ())

  ;; void glFlush( void )
  (define-function void glFlush ())

  ;; void glHint( GLenum target, GLenum mode )
  (define-function void glHint (int int))

  ;;;; Depth Buffer

  ;; void glClearDepth( GLclampd depth )
  (define-function void glClearDepth (double))

  ;; void glDepthFunc( GLenum func )
  (define-function void glDepthFunc (int))

  ;; void glDepthMask( GLboolean flag )
  (define-function void glDepthMask (int))

  ;; void glDepthRange( GLclampd near_val, GLclampd far_val )  
  (define-function void glDepthRange (double double))

  ;;;; Accumulation Buffer

  ;; void glClearAccum( GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha )
  (define-function void glClearAccum (float float float float))

  ;; void glAccum( GLenum op, GLfloat value )
  (define-function void glAccum (int float))

  ;;;; Transformation

  ;; void glMatrixMode( GLenum mode )
  (define-function void glMatrixMode (int))

  ;; void glOrtho( GLdouble left, GLdouble right, GLdouble bottom, GLdouble top, GLdouble near_val, GLdouble far_val )
  (define-function void glOrtho (double double double double double double))

  ;; void glFrustum( GLdouble left, GLdouble right, GLdouble bottom, GLdouble top, GLdouble near_val, GLdouble far_val )
  (define-function void glFrustum (double double double double double double))

  ;; void glViewport( GLint x, GLint y, GLsizei width, GLsizei height )
  (define-function void glViewport (int int int int))

  ;; void glPushMatrix( void )
  (define-function void glPushMatrix ())

  ;; void glPopMatrix( void )
  (define-function void glPopMatrix ())

  ;; void glLoadIdentity( void )
  (define-function void glLoadIdentity ())

  ;; void glLoadMatrixd( const GLdouble *m )
  (define-function void glLoadMatrixd (byte*))

  ;; void glLoadMatrixf( const GLfloat *m )
  (define-function void glLoadMatrixf (byte*))

  ;; void glMultMatrixd( const GLdouble *m )
  (define-function void glMultMatrixd (byte*))

  ;; void glMultMatrixf( const GLfloat *m )
  (define-function void glMultMatrixf (byte*))

  ;; void glRotated( GLdouble angle, GLdouble x, GLdouble y, GLdouble z )
  (define-function void glRotated (double double double double))

  ;; void glRotatef( GLfloat angle, GLfloat x, GLfloat y, GLfloat z )
  (define-function void glRotatef (float float float float))

  ;; void glScaled( GLdouble x, GLdouble y, GLdouble z )
  (define-function void glScaled (double double double))

  ;; void glScalef( GLfloat x, GLfloat y, GLfloat z )
  (define-function void glScalef (float float float))

  ;; void glTranslated( GLdouble x, GLdouble y, GLdouble z )
  (define-function void glTranslated (double double double))

  ;; void glTranslatef( GLfloat x, GLfloat y, GLfloat z )
  (define-function void glTranslatef (float float float))

  ;;;; Display Lists

  ;; GLboolean glIsList( GLuint list )
  (define-function int glIsList (int))

  ;; void glDeleteLists( GLuint list, GLsizei range )
  (define-function void glDeleteLists (int int))

  ;; GLuint glGenLists( GLsizei range )
  (define-function int glGenLists (int))

  ;; void glNewList( GLuint list, GLenum mode )
  (define-function void glNewList (int int))

  ;; void glEndList( void )
  (define-function void glEndList ())

  ;; void glCallList( GLuint list )
  (define-function void glCallList (int))

  ;; void glCallLists( GLsizei n, GLenum type, const GLvoid *lists )
  (define-function void glCallLists (int int byte*))

  ;; void glListBase( GLuint base )
  (define-function void glListBase (int))

  ;;;; Drawing Functions

  ;; void glBegin( GLenum mode )
  (define-function void glBegin (int))

  ;; void glEnd( void )
  (define-function void glEnd ())

  ;; void glVertex2d( GLdouble x, GLdouble y )
  (define-function void glVertex2d (double double))

  ;; void glVertex2f( GLfloat x, GLfloat y )
  (define-function void glVertex2f (float float))

  ;; void glVertex2i( GLint x, GLint y )
  (define-function void glVertex2i (int int))

  ;; void glVertex2s( GLshort x, GLshort y )
  (define-function void glVertex2s (int int))

  ;; void glVertex3d( GLdouble x, GLdouble y, GLdouble z )
  (define-function void glVertex3d (double double double))

  ;; void glVertex3f( GLfloat x, GLfloat y, GLfloat z )
  (define-function void glVertex3f (float float float))

  ;; void glVertex3i( GLint x, GLint y, GLint z )
  (define-function void glVertex3i (int int int))

  ;; void glVertex3s( GLshort x, GLshort y, GLshort z )
  (define-function void glVertex3s (int int int))

  ;; void glVertex4d( GLdouble x, GLdouble y, GLdouble z, GLdouble w )
  (define-function void glVertex4d (double double double double))

  ;; void glVertex4f( GLfloat x, GLfloat y, GLfloat z, GLfloat w )
  (define-function void glVertex4f (float float float float))

  ;; void glVertex4i( GLint x, GLint y, GLint z, GLint w )
  (define-function void glVertex4i (int int int int))

  ;; void glVertex4s( GLshort x, GLshort y, GLshort z, GLshort w )
  (define-function void glVertex4s (int int int int))

  ;; void glVertex2dv( const GLdouble *v )
  (define-function void glVertex2dv (byte*))

  ;; void glVertex2fv( const GLfloat *v )
  (define-function void glVertex2fv (byte*))

  ;; void glVertex2iv( const GLint *v )
  (define-function void glVertex2iv (byte*))

  ;; void glVertex2sv( const GLshort *v )
  (define-function void glVertex2sv (byte*))

  ;; void glVertex3dv( const GLdouble *v )
  (define-function void glVertex3dv (byte*))

  ;; void glVertex3fv( const GLfloat *v )
  (define-function void glVertex3fv (byte*))

  ;; void glVertex3iv( const GLint *v )
  (define-function void glVertex3iv (byte*))

  ;; void glVertex3sv( const GLshort *v )
  (define-function void glVertex3sv (byte*))

  ;; void glVertex4dv( const GLdouble *v )
  (define-function void glVertex4dv (byte*))

  ;; void glVertex4fv( const GLfloat *v )
  (define-function void glVertex4fv (byte*))

  ;; void glVertex4iv( const GLint *v )
  (define-function void glVertex4iv (byte*))

  ;; void glVertex4sv( const GLshort *v )
  (define-function void glVertex4sv (byte*))

  ;; void glNormal3b( GLbyte nx, GLbyte ny, GLbyte nz )
  (define-function void glNormal3b (int int int))

  ;; void glNormal3d( GLdouble nx, GLdouble ny, GLdouble nz )
  (define-function void glNormal3d (double double double))

  ;; void glNormal3f( GLfloat nx, GLfloat ny, GLfloat nz )
  (define-function void glNormal3f (float float float))

  ;; void glNormal3i( GLint nx, GLint ny, GLint nz )
  (define-function void glNormal3i (int int int))

  ;; void glNormal3s( GLshort nx, GLshort ny, GLshort nz )
  (define-function void glNormal3s (int int int))

  ;; void glNormal3bv( const GLbyte *v )
  (define-function void glNormal3bv (byte*))

  ;; void glNormal3dv( const GLdouble *v )
  (define-function void glNormal3dv (byte*))

  ;; void glNormal3fv( const GLfloat *v )
  (define-function void glNormal3fv (byte*))

  ;; void glNormal3iv( const GLint *v )
  (define-function void glNormal3iv (byte*))

  ;; void glNormal3sv( const GLshort *v )
  (define-function void glNormal3sv (byte*))

  ;; void glIndexd( GLdouble c )
  (define-function void glIndexd (double))

  ;; void glIndexf( GLfloat c )
  (define-function void glIndexf (float))

  ;; void glIndexi( GLint c )
  (define-function void glIndexi (int))

  ;; void glIndexs( GLshort c )
  (define-function void glIndexs (int))

  ;; void glIndexub( GLubyte c )  /* 1.1 */
  (define-function void glIndexub (int))

  ;; void glIndexdv( const GLdouble *c )
  (define-function void glIndexdv (byte*))

  ;; void glIndexfv( const GLfloat *c )
  (define-function void glIndexfv (byte*))

  ;; void glIndexiv( const GLint *c )
  (define-function void glIndexiv (byte*))

  ;; void glIndexsv( const GLshort *c )
  (define-function void glIndexsv (byte*))

  ;; void glIndexubv( const GLubyte *c )  /* 1.1 */
  (define-function void glIndexubv (byte*))

  ;; void glColor3b( GLbyte red, GLbyte green, GLbyte blue )
  (define-function void glColor3b (int int int))

  ;; void glColor3d( GLdouble red, GLdouble green, GLdouble blue )
  (define-function void glColor3d (double double double))

  ;; void glColor3f( GLfloat red, GLfloat green, GLfloat blue )
  (define-function void glColor3f (float float float))

  ;; void glColor3i( GLint red, GLint green, GLint blue )
  (define-function void glColor3i (int int int))

  ;; void glColor3s( GLshort red, GLshort green, GLshort blue )
  (define-function void glColor3s (int int int))

  ;; void glColor3ub( GLubyte red, GLubyte green, GLubyte blue )
  (define-function void glColor3ub (int int int))

  ;; void glColor3ui( GLuint red, GLuint green, GLuint blue )
  (define-function void glColor3ui (int int int))

  ;; void glColor3us( GLushort red, GLushort green, GLushort blue )
  (define-function void glColor3us (int int int))

  ;; void glColor4b( GLbyte red, GLbyte green, GLbyte blue, GLbyte alpha )
  (define-function void glColor4b (int int int int))

  ;; void glColor4d( GLdouble red, GLdouble green, GLdouble blue, GLdouble alpha )
  (define-function void glColor4d (double double double double))

  ;; void glColor4f( GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha )
  (define-function void glColor4f (float float float float))

  ;; void glColor4i( GLint red, GLint green, GLint blue, GLint alpha )
  (define-function void glColor4i (int int int int))

  ;; void glColor4s( GLshort red, GLshort green, GLshort blue, GLshort alpha )
  (define-function void glColor4s (int int int int))

  ;; void glColor4ub( GLubyte red, GLubyte green, GLubyte blue, GLubyte alpha )
  (define-function void glColor4ub (int int int int))

  ;; void glColor4ui( GLuint red, GLuint green, GLuint blue, GLuint alpha )
  (define-function void glColor4ui (int int int int))

  ;; void glColor4us( GLushort red, GLushort green, GLushort blue, GLushort alpha )
  (define-function void glColor4us (int int int int))

  ;; void glColor3bv( const GLbyte *v )
  (define-function void glColor3bv (byte*))

  ;; void glColor3dv( const GLdouble *v )
  (define-function void glColor3dv (byte*))

  ;; void glColor3fv( const GLfloat *v )
  (define-function void glColor3fv (byte*))

  ;; void glColor3iv( const GLint *v )
  (define-function void glColor3iv (byte*))

  ;; void glColor3sv( const GLshort *v )
  (define-function void glColor3sv (byte*))

  ;; void glColor3ubv( const GLubyte *v )
  (define-function void glColor3ubv (byte*))

  ;; void glColor3uiv( const GLuint *v )
  (define-function void glColor3uiv (byte*))

  ;; void glColor3usv( const GLushort *v )
  (define-function void glColor3usv (byte*))

  ;; void glColor4bv( const GLbyte *v )
  (define-function void glColor4bv (byte*))

  ;; void glColor4dv( const GLdouble *v )
  (define-function void glColor4dv (byte*))

  ;; void glColor4fv( const GLfloat *v )
  (define-function void glColor4fv (byte*))

  ;; void glColor4iv( const GLint *v )
  (define-function void glColor4iv (byte*))

  ;; void glColor4sv( const GLshort *v )
  (define-function void glColor4sv (byte*))

  ;; void glColor4ubv( const GLubyte *v )
  (define-function void glColor4ubv (byte*))

  ;; void glColor4uiv( const GLuint *v )
  (define-function void glColor4uiv (byte*))

  ;; void glColor4usv( const GLushort *v )
  (define-function void glColor4usv (byte*))

  ;; void glTexCoord1d( GLdouble s )
  (define-function void glTexCoord1d (double))

  ;; void glTexCoord1f( GLfloat s )
  (define-function void glTexCoord1f (float))

  ;; void glTexCoord1i( GLint s )
  (define-function void glTexCoord1i (int))

  ;; void glTexCoord1s( GLshort s )
  (define-function void glTexCoord1s (int))

  ;; void glTexCoord2d( GLdouble s, GLdouble t )
  (define-function void glTexCoord2d (double double))

  ;; void glTexCoord2f( GLfloat s, GLfloat t )
  (define-function void glTexCoord2f (float float))

  ;; void glTexCoord2i( GLint s, GLint t )
  (define-function void glTexCoord2i (int int))

  ;; void glTexCoord2s( GLshort s, GLshort t )
  (define-function void glTexCoord2s (int int))

  ;; void glTexCoord3d( GLdouble s, GLdouble t, GLdouble r )
  (define-function void glTexCoord3d (double double double))

  ;; void glTexCoord3f( GLfloat s, GLfloat t, GLfloat r )
  (define-function void glTexCoord3f (float float float))

  ;; void glTexCoord3i( GLint s, GLint t, GLint r )
  (define-function void glTexCoord3i (int int int))

  ;; void glTexCoord3s( GLshort s, GLshort t, GLshort r )
  (define-function void glTexCoord3s (int int int))

  ;; void glTexCoord4d( GLdouble s, GLdouble t, GLdouble r, GLdouble q )
  (define-function void glTexCoord4d (double double double double))

  ;; void glTexCoord4f( GLfloat s, GLfloat t, GLfloat r, GLfloat q )
  (define-function void glTexCoord4f (float float float float))

  ;; void glTexCoord4i( GLint s, GLint t, GLint r, GLint q )
  (define-function void glTexCoord4i (int int int int))

  ;; void glTexCoord4s( GLshort s, GLshort t, GLshort r, GLshort q )
  (define-function void glTexCoord4s (int int int int))

  ;; void glTexCoord1dv( const GLdouble *v )
  (define-function void glTexCoord1dv (byte*))

  ;; void glTexCoord1fv( const GLfloat *v )
  (define-function void glTexCoord1fv (byte*))

  ;; void glTexCoord1iv( const GLint *v )
  (define-function void glTexCoord1iv (byte*))

  ;; void glTexCoord1sv( const GLshort *v )
  (define-function void glTexCoord1sv (byte*))

  ;; void glTexCoord2dv( const GLdouble *v )
  (define-function void glTexCoord2dv (byte*))

  ;; void glTexCoord2fv( const GLfloat *v )
  (define-function void glTexCoord2fv (byte*))

  ;; void glTexCoord2iv( const GLint *v )
  (define-function void glTexCoord2iv (byte*))

  ;; void glTexCoord2sv( const GLshort *v )
  (define-function void glTexCoord2sv (byte*))

  ;; void glTexCoord3dv( const GLdouble *v )
  (define-function void glTexCoord3dv (byte*))

  ;; void glTexCoord3fv( const GLfloat *v )
  (define-function void glTexCoord3fv (byte*))

  ;; void glTexCoord3iv( const GLint *v )
  (define-function void glTexCoord3iv (byte*))

  ;; void glTexCoord3sv( const GLshort *v )
  (define-function void glTexCoord3sv (byte*))

  ;; void glTexCoord4dv( const GLdouble *v )
  (define-function void glTexCoord4dv (byte*))

  ;; void glTexCoord4fv( const GLfloat *v )
  (define-function void glTexCoord4fv (byte*))

  ;; void glTexCoord4iv( const GLint *v )
  (define-function void glTexCoord4iv (byte*))

  ;; void glTexCoord4sv( const GLshort *v )
  (define-function void glTexCoord4sv (byte*))

  ;; void glRasterPos2d( GLdouble x, GLdouble y )
  (define-function void glRasterPos2d (double double))

  ;; void glRasterPos2f( GLfloat x, GLfloat y )
  (define-function void glRasterPos2f (float float))

  ;; void glRasterPos2i( GLint x, GLint y )
  (define-function void glRasterPos2i (int int))

  ;; void glRasterPos2s( GLshort x, GLshort y )
  (define-function void glRasterPos2s (int int))

  ;; void glRasterPos3d( GLdouble x, GLdouble y, GLdouble z )
  (define-function void glRasterPos3d (double double double))

  ;; void glRasterPos3f( GLfloat x, GLfloat y, GLfloat z )
  (define-function void glRasterPos3f (float float float))

  ;; void glRasterPos3i( GLint x, GLint y, GLint z )
  (define-function void glRasterPos3i (int int int))

  ;; void glRasterPos3s( GLshort x, GLshort y, GLshort z )
  (define-function void glRasterPos3s (int int int))

  ;; void glRasterPos4d( GLdouble x, GLdouble y, GLdouble z, GLdouble w )
  (define-function void glRasterPos4d (double double double double))

  ;; void glRasterPos4f( GLfloat x, GLfloat y, GLfloat z, GLfloat w )
  (define-function void glRasterPos4f (float float float float))

  ;; void glRasterPos4i( GLint x, GLint y, GLint z, GLint w )
  (define-function void glRasterPos4i (int int int int))

  ;; void glRasterPos4s( GLshort x, GLshort y, GLshort z, GLshort w )
  (define-function void glRasterPos4s (int int int int))

  ;; void glRasterPos2dv( const GLdouble *v )
  (define-function void glRasterPos2dv (byte*))

  ;; void glRasterPos2fv( const GLfloat *v )
  (define-function void glRasterPos2fv (byte*))

  ;; void glRasterPos2iv( const GLint *v )
  (define-function void glRasterPos2iv (byte*))

  ;; void glRasterPos2sv( const GLshort *v )
  (define-function void glRasterPos2sv (byte*))

  ;; void glRasterPos3dv( const GLdouble *v )
  (define-function void glRasterPos3dv (byte*))

  ;; void glRasterPos3fv( const GLfloat *v )
  (define-function void glRasterPos3fv (byte*))

  ;; void glRasterPos3iv( const GLint *v )
  (define-function void glRasterPos3iv (byte*))

  ;; void glRasterPos3sv( const GLshort *v )
  (define-function void glRasterPos3sv (byte*))

  ;; void glRasterPos4dv( const GLdouble *v )
  (define-function void glRasterPos4dv (byte*))

  ;; void glRasterPos4fv( const GLfloat *v )
  (define-function void glRasterPos4fv (byte*))

  ;; void glRasterPos4iv( const GLint *v )
  (define-function void glRasterPos4iv (byte*))

  ;; void glRasterPos4sv( const GLshort *v )
  (define-function void glRasterPos4sv (byte*))

  ;; void glRectd( GLdouble x1, GLdouble y1, GLdouble x2, GLdouble y2 )
  (define-function void glRectd (double double double double))

  ;; void glRectf( GLfloat x1, GLfloat y1, GLfloat x2, GLfloat y2 )
  (define-function void glRectf (float float float float))

  ;; void glRecti( GLint x1, GLint y1, GLint x2, GLint y2 )
  (define-function void glRecti (int int int int))

  ;; void glRects( GLshort x1, GLshort y1, GLshort x2, GLshort y2 )
  (define-function void glRects (int int int int))

  ;; void glRectdv( const GLdouble *v1, const GLdouble *v2 )
  (define-function void glRectdv (byte* byte*))

  ;; void glRectfv( const GLfloat *v1, const GLfloat *v2 )
  (define-function void glRectfv (byte* byte*))

  ;; void glRectiv( const GLint *v1, const GLint *v2 )
  (define-function void glRectiv (byte* byte*))

  ;; void glRectsv( const GLshort *v1, const GLshort *v2 )
  (define-function void glRectsv (byte* byte*))

  ;;;; Vertex Arrays 1.1

  ;; void glVertexPointer( GLint size, GLenum type, GLsizei stride, const GLvoid *ptr )
  (define-function void glVertexPointer (int int int byte*))

  ;; void glNormalPointer( GLenum type, GLsizei stride, const GLvoid *ptr )
  (define-function void glNormalPointer (int int byte*))

  ;; void glColorPointer( GLint size, GLenum type, GLsizei stride, const GLvoid *ptr )
  (define-function void glColorPointer (int int int byte*))

  ;; void glIndexPointer( GLenum type, GLsizei stride, const GLvoid *ptr )
  (define-function void glIndexPointer (int int byte*))

  ;; void glTexCoordPointer( GLint size, GLenum type, GLsizei stride, const GLvoid *ptr )
  (define-function void glTexCoordPointer (int int int byte*))

  ;; void glEdgeFlagPointer( GLsizei stride, const GLvoid *ptr )
  (define-function void glEdgeFlagPointer (int byte*))

  ;; void glGetPointerv( GLenum pname, GLvoid **params )
  (define-function void glGetPointerv (int byte*))

  ;; void glArrayElement( GLint i )
  (define-function void glArrayElement (int))

  ;; void glDrawArrays( GLenum mode, GLint first, GLsizei count )
  (define-function void glDrawArrays (int int int))

  ;; void glDrawElements( GLenum mode, GLsizei count, GLenum type, const GLvoid *indices )
  (define-function void glDrawElements (int int int byte*))

  ;; void glInterleavedArrays( GLenum format, GLsizei stride, const GLvoid *pointer )
  (define-function void glInterleavedArrays (int int byte*))

  ;;;; Lighting

  ;; void glShadeModel( GLenum mode )
  (define-function void glShadeModel (int))

  ;; void glLightf( GLenum light, GLenum pname, GLfloat param )
  (define-function void glLightf (int int float))

  ;; void glLighti( GLenum light, GLenum pname, GLint param )
  (define-function void glLighti (int int int))

  ;; void glLightfv( GLenum light, GLenum pname, const GLfloat *params )
  (define-function void glLightfv (int int byte*))

  ;; void glLightiv( GLenum light, GLenum pname, const GLint *params )
  (define-function void glLightiv (int int byte*))

  ;; void glGetLightfv( GLenum light, GLenum pname, GLfloat *params )
  (define-function void glGetLightfv (int int byte*))

  ;; void glGetLightiv( GLenum light, GLenum pname, GLint *params )
  (define-function void glGetLightiv (int int byte*))

  ;; void glLightModelf( GLenum pname, GLfloat param )
  (define-function void glLightModelf (int float))

  ;; void glLightModeli( GLenum pname, GLint param )
  (define-function void glLightModeli (int int))

  ;; void glLightModelfv( GLenum pname, const GLfloat *params )
  (define-function void glLightModelfv (int byte*))

  ;; void glLightModeliv( GLenum pname, const GLint *params )
  (define-function void glLightModeliv (int byte*))

  ;; void glMaterialf( GLenum face, GLenum pname, GLfloat param )
  (define-function void glMaterialf (int int float))

  ;; void glMateriali( GLenum face, GLenum pname, GLint param )
  (define-function void glMateriali (int int int))

  ;; void glMaterialfv( GLenum face, GLenum pname, const GLfloat *params )
  (define-function void glMaterialfv (int int byte*))

  ;; void glMaterialiv( GLenum face, GLenum pname, const GLint *params )
  (define-function void glMaterialiv (int int byte*))

  ;; void glGetMaterialfv( GLenum face, GLenum pname, GLfloat *params )
  (define-function void glGetMaterialfv (int int byte*))

  ;; void glGetMaterialiv( GLenum face, GLenum pname, GLint *params )
  (define-function void glGetMaterialiv (int int byte*))

  ;; void glColorMaterial( GLenum face, GLenum mode )
  (define-function void glColorMaterial (int int))

  ;;;; Raster functions

  ;; void glPixelZoom( GLfloat xfactor, GLfloat yfactor )
  (define-function void glPixelZoom (float float))

  ;; void glPixelStoref( GLenum pname, GLfloat param )
  (define-function void glPixelStoref (int float))

  ;; void glPixelStorei( GLenum pname, GLint param )
  (define-function void glPixelStorei (int int))

  ;; void glPixelTransferf( GLenum pname, GLfloat param )
  (define-function void glPixelTransferf (int float))

  ;; void glPixelTransferi( GLenum pname, GLint param )
  (define-function void glPixelTransferi (int int))

  ;; void glPixelMapfv( GLenum map, GLsizei mapsize, const GLfloat *values )
  (define-function void glPixelMapfv (int int byte*))

  ;; void glPixelMapuiv( GLenum map, GLsizei mapsize, const GLuint *values )
  (define-function void glPixelMapuiv (int int byte*))

  ;; void glPixelMapusv( GLenum map, GLsizei mapsize, const GLushort *values )
  (define-function void glPixelMapusv (int int byte*))

  ;; void glGetPixelMapfv( GLenum map, GLfloat *values )
  (define-function void glGetPixelMapfv (int byte*))

  ;; void glGetPixelMapuiv( GLenum map, GLuint *values )
  (define-function void glGetPixelMapuiv (int byte*))

  ;; void glGetPixelMapusv( GLenum map, GLushort *values )
  (define-function void glGetPixelMapusv (int byte*))

  ;; void glBitmap( GLsizei width, GLsizei height, GLfloat xorig, GLfloat yorig, GLfloat xmove, GLfloat ymove, const GLubyte *bitmap )
  (define-function void glBitmap (int int float float float float byte*))

  ;; void glReadPixels( GLint x, GLint y, GLsizei width, GLsizei height, GLenum format, GLenum type, GLvoid *pixels )
  (define-function void glReadPixels (int int int int int int byte*))

  ;; void glDrawPixels( GLsizei width, GLsizei height, GLenum format, GLenum type, const GLvoid *pixels )
  (define-function void glDrawPixels (int int int int byte*))

  ;; void glCopyPixels( GLint x, GLint y, GLsizei width, GLsizei height, GLenum type )
  (define-function void glCopyPixels (int int int int int))

  ;;;; Stenciling

  ;; void glStencilFunc( GLenum func, GLint ref, GLuint mask )
  (define-function void glStencilFunc (int int int))

  ;; void glStencilMask( GLuint mask )
  (define-function void glStencilMask (int))

  ;; void glStencilOp( GLenum fail, GLenum zfail, GLenum zpass )
  (define-function void glStencilOp (int int int))

  ;; void glClearStencil( GLint s )
  (define-function void glClearStencil (int))

  ;;;; Texture mapping

  ;; void glTexGend( GLenum coord, GLenum pname, GLdouble param )
  (define-function void glTexGend (int int double))

  ;; void glTexGenf( GLenum coord, GLenum pname, GLfloat param )
  (define-function void glTexGenf (int int float))

  ;; void glTexGeni( GLenum coord, GLenum pname, GLint param )
  (define-function void glTexGeni (int int int))

  ;; void glTexGendv( GLenum coord, GLenum pname, const GLdouble *params )
  (define-function void glTexGendv (int int byte*))

  ;; void glTexGenfv( GLenum coord, GLenum pname, const GLfloat *params )
  (define-function void glTexGenfv (int int byte*))

  ;; void glTexGeniv( GLenum coord, GLenum pname, const GLint *params )
  (define-function void glTexGeniv (int int byte*))

  ;; void glGetTexGendv( GLenum coord, GLenum pname, GLdouble *params )
  (define-function void glGetTexGendv (int int byte*))

  ;; void glGetTexGenfv( GLenum coord, GLenum pname, GLfloat *params )
  (define-function void glGetTexGenfv (int int byte*))

  ;; void glGetTexGeniv( GLenum coord, GLenum pname, GLint *params )
  (define-function void glGetTexGeniv (int int byte*))

  ;; void glTexEnvf( GLenum target, GLenum pname, GLfloat param )
  (define-function void glTexEnvf (int int float))

  ;; void glTexEnvi( GLenum target, GLenum pname, GLint param )
  (define-function void glTexEnvi (int int int))

  ;; void glTexEnvfv( GLenum target, GLenum pname, const GLfloat *params )
  (define-function void glTexEnvfv (int int byte*))

  ;; void glTexEnviv( GLenum target, GLenum pname, const GLint *params )
  (define-function void glTexEnviv (int int byte*))

  ;; void glGetTexEnvfv( GLenum target, GLenum pname, GLfloat *params )
  (define-function void glGetTexEnvfv (int int byte*))

  ;; void glGetTexEnviv( GLenum target, GLenum pname, GLint *params )
  (define-function void glGetTexEnviv (int int byte*))

  ;; void glTexParameterf( GLenum target, GLenum pname, GLfloat param )
  (define-function void glTexParameterf (int int float))

  ;; void glTexParameteri( GLenum target, GLenum pname, GLint param )
  (define-function void glTexParameteri (int int int))

  ;; void glTexParameterfv( GLenum target, GLenum pname, const GLfloat *params )
  (define-function void glTexParameterfv (int int byte*))

  ;; void glTexParameteriv( GLenum target, GLenum pname, const GLint *params )
  (define-function void glTexParameteriv (int int byte*))

  ;; void glGetTexParameterfv( GLenum target, GLenum pname, GLfloat *params)
  (define-function void glGetTexParameterfv (int int byte*))

  ;; void glGetTexParameteriv( GLenum target, GLenum pname, GLint *params )
  (define-function void glGetTexParameteriv (int int byte*))

  ;; void glGetTexLevelParameterfv( GLenum target, GLint level, GLenum pname, GLfloat *params )
  (define-function void glGetTexLevelParameterfv (int int int byte*))

  ;; void glGetTexLevelParameteriv( GLenum target, GLint level, GLenum pname, GLint *params )
  (define-function void glGetTexLevelParameteriv (int int int byte*))

  ;; void glTexImage1D( GLenum target, GLint level, GLint internalFormat, GLsizei width, GLint border, GLenum format, GLenum type, const GLvoid *pixels )
  (define-function void glTexImage1D (int int int int int int int byte*))

  ;; void glTexImage2D( GLenum target, GLint level, GLint internalFormat, GLsizei width, GLsizei height, GLint border, GLenum format, GLenum type, const GLvoid *pixels )
  (define-function void glTexImage2D (int int int int int int int int byte*))

  ;; void glGetTexImage( GLenum target, GLint level, GLenum format, GLenum type, GLvoid *pixels )
  (define-function void glGetTexImage (int int int int byte*))

  ;;;; 1.1 functions

  ;; void glGenTextures( GLsizei n, GLuint *textures )
  (define-function void glGenTextures (int byte*))

  ;; void glDeleteTextures( GLsizei n, const GLuint *textures)
  (define-function void glDeleteTextures (int byte*))

  ;; void glBindTexture( GLenum target, GLuint texture )
  (define-function void glBindTexture (int int))

  ;; void glPrioritizeTextures( GLsizei n, const GLuint *textures, const GLclampf *priorities )
  (define-function void glPrioritizeTextures (int byte* byte*))

  ;; GLboolean glAreTexturesResident( GLsizei n, const GLuint *textures, GLboolean *residences )
  (define-function int glAreTexturesResident (int byte* byte*))

  ;; GLboolean glIsTexture( GLuint texture )
  (define-function int glIsTexture (int))

  ;; void glTexSubImage1D( GLenum target, GLint level, GLint xoffset, GLsizei width, GLenum format, GLenum type, const GLvoid *pixels )
  (define-function void glTexSubImage1D (int int int int int int byte*))

  ;; void glTexSubImage2D( GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLenum type, const GLvoid *pixels )
  (define-function void glTexSubImage2D (int int int int int int int int byte*))

  ;; void glCopyTexImage1D( GLenum target, GLint level, GLenum internalformat, GLint x, GLint y, GLsizei width, GLint border )
  (define-function void glCopyTexImage1D (int int int int int int int))

  ;; void glCopyTexImage2D( GLenum target, GLint level, GLenum internalformat, GLint x, GLint y, GLsizei width, GLsizei height, GLint border )
  (define-function void glCopyTexImage2D (int int int int int int int int))

  ;; void glCopyTexSubImage1D( GLenum target, GLint level, GLint xoffset, GLint x, GLint y, GLsizei width )
  (define-function void glCopyTexSubImage1D (int int int int int int))

  ;; void glCopyTexSubImage2D( GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint x, GLint y, GLsizei width, GLsizei height )
  (define-function void glCopyTexSubImage2D (int int int int int int int int))

  ;;;; Evaluators

  ;; void glMap1d( GLenum target, GLdouble u1, GLdouble u2, GLint stride, GLint order, const GLdouble *points )
  (define-function void glMap1d (int double double int int byte*))

  ;; void glMap1f( GLenum target, GLfloat u1, GLfloat u2, GLint stride, GLint order, const GLfloat *points )
  (define-function void glMap1f (int float float int int byte*))

  ;; void glMap2d( GLenum target, GLdouble u1, GLdouble u2, GLint ustride, GLint uorder, GLdouble v1, GLdouble v2, GLint vstride, GLint vorder, const GLdouble *points )
  (define-function void glMap2d (int double double int int double double int int byte*))

  ;; void glMap2f( GLenum target, GLfloat u1, GLfloat u2, GLint ustride, GLint uorder, GLfloat v1, GLfloat v2, GLint vstride, GLint vorder, const GLfloat *points )
  (define-function void glMap2f (int float float int int float float int int byte*))

  ;; void glGetMapdv( GLenum target, GLenum query, GLdouble *v )
  (define-function void glGetMapdv (int int byte*))

  ;; void glGetMapfv( GLenum target, GLenum query, GLfloat *v )
  (define-function void glGetMapfv (int int byte*))

  ;; void glGetMapiv( GLenum target, GLenum query, GLint *v )
  (define-function void glGetMapiv (int int byte*))

  ;; void glEvalCoord1d( GLdouble u )
  (define-function void glEvalCoord1d (double))

  ;; void glEvalCoord1f( GLfloat u )
  (define-function void glEvalCoord1f (float))

  ;; void glEvalCoord1dv( const GLdouble *u )
  (define-function void glEvalCoord1dv (byte*))

  ;; void glEvalCoord1fv( const GLfloat *u )
  (define-function void glEvalCoord1fv (byte*))

  ;; void glEvalCoord2d( GLdouble u, GLdouble v )
  (define-function void glEvalCoord2d (double double))

  ;; void glEvalCoord2f( GLfloat u, GLfloat v )
  (define-function void glEvalCoord2f (float float))

  ;; void glEvalCoord2dv( const GLdouble *u )
  (define-function void glEvalCoord2dv (byte*))

  ;; void glEvalCoord2fv( const GLfloat *u )
  (define-function void glEvalCoord2fv (byte*))

  ;; void glMapGrid1d( GLint un, GLdouble u1, GLdouble u2 )
  (define-function void glMapGrid1d (int double double))

  ;; void glMapGrid1f( GLint un, GLfloat u1, GLfloat u2 )
  (define-function void glMapGrid1f (int float float))

  ;; void glMapGrid2d( GLint un, GLdouble u1, GLdouble u2, GLint vn, GLdouble v1, GLdouble v2 )
  (define-function void glMapGrid2d (int double double int double double))

  ;; void glMapGrid2f( GLint un, GLfloat u1, GLfloat u2, GLint vn, GLfloat v1, GLfloat v2 )
  (define-function void glMapGrid2f (int float float int float float))

  ;; void glEvalPoint1( GLint i )
  (define-function void glEvalPoint1 (int))

  ;; void glEvalPoint2( GLint i, GLint j )
  (define-function void glEvalPoint2 (int int))

  ;; void glEvalMesh1( GLenum mode, GLint i1, GLint i2 )
  (define-function void glEvalMesh1 (int int int))

  ;; void glEvalMesh2( GLenum mode, GLint i1, GLint i2, GLint j1, GLint j2 )
  (define-function void glEvalMesh2 (int int int int int))

  ;;;; Fog

  ;; void glFogf( GLenum pname, GLfloat param )
  (define-function void glFogf (int float))

  ;; void glFogi( GLenum pname, GLint param )
  (define-function void glFogi (int int))

  ;; void glFogfv( GLenum pname, const GLfloat *params )
  (define-function void glFogfv (int byte*))

  ;; void glFogiv( GLenum pname, const GLint *params )
  (define-function void glFogiv (int byte*))

  ;;;; Selection and Feedback

  ;; void glFeedbackBuffer( GLsizei size, GLenum type, GLfloat *buffer )
  (define-function void glFeedbackBuffer (int int byte*))

  ;; void glPassThrough( GLfloat token )
  (define-function void glPassThrough (float))

  ;; void glSelectBuffer( GLsizei size, GLuint *buffer )
  (define-function void glSelectBuffer (int byte*))

  ;; void glInitNames( void )
  (define-function void glInitNames ())

  ;; void glLoadName( GLuint name )
  (define-function void glLoadName (int))

  ;; void glPushName( GLuint name )
  (define-function void glPushName (int))

  ;; void glPopName( void )
  (define-function void glPopName ())

  ;;;; OpenGL 1.2

  ;; void glDrawRangeElements( GLenum mode, GLuint start, GLuint end, GLsizei count, GLenum type, const GLvoid *indices )
  (define-function void glDrawRangeElements (int int int int int byte*))

  ;; void glTexImage3D( GLenum target, GLint level, GLint internalFormat, GLsizei width, GLsizei height, GLsizei depth, GLint border, GLenum format, GLenum type, const GLvoid *pixels )
  (define-function void glTexImage3D (int int int int int int int int int byte*))

  ;; void glTexSubImage3D( GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLenum type, const GLvoid *pixels)
  (define-function void glTexSubImage3D (int int int int int int int int int int byte*))

  ;; void glCopyTexSubImage3D( GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLint x, GLint y, GLsizei width, GLsizei height )
  (define-function void glCopyTexSubImage3D (int int int int int int int int int))

  ;;;; GL_ARB_imaging

  ;; void glColorTable( GLenum target, GLenum internalformat, GLsizei width, GLenum format, GLenum type, const GLvoid *table )
  (define-function void glColorTable (int int int int int byte*))

  ;; void glColorSubTable( GLenum target, GLsizei start, GLsizei count, GLenum format, GLenum type, const GLvoid *data )
  (define-function void glColorSubTable (int int int int int byte*))

  ;; void glColorTableParameteriv(GLenum target, GLenum pname, const GLint *params)
  (define-function void glColorTableParameteriv (int int byte*))

  ;; void glColorTableParameterfv(GLenum target, GLenum pname, const GLfloat *params)
  (define-function void glColorTableParameterfv (int int byte*))

  ;; void glCopyColorSubTable( GLenum target, GLsizei start, GLint x, GLint y, GLsizei width )
  (define-function void glCopyColorSubTable (int int int int int))

  ;; void glCopyColorTable( GLenum target, GLenum internalformat, GLint x, GLint y, GLsizei width )
  (define-function void glCopyColorTable (int int int int int))

  ;; void glGetColorTable( GLenum target, GLenum format, GLenum type, GLvoid *table )
  (define-function void glGetColorTable (int int int byte*))

  ;; void glGetColorTableParameterfv( GLenum target, GLenum pname, GLfloat *params )
  (define-function void glGetColorTableParameterfv (int int byte*))

  ;; void glGetColorTableParameteriv( GLenum target, GLenum pname, GLint *params )
  (define-function void glGetColorTableParameteriv (int int byte*))

  ;; void glBlendEquation( GLenum mode )
  (define-function void glBlendEquation (int))

  ;; void glBlendColor( GLclampf red, GLclampf green, GLclampf blue, GLclampf alpha )
  (define-function void glBlendColor (float float float float))

  ;; void glHistogram( GLenum target, GLsizei width, GLenum internalformat, GLboolean sink )
  (define-function void glHistogram (int int int int))

  ;; void glResetHistogram( GLenum target )
  (define-function void glResetHistogram (int))

  ;; void glGetHistogram( GLenum target, GLboolean reset, GLenum format, GLenum type, GLvoid *values )
  (define-function void glGetHistogram (int int int int byte*))

  ;; void glGetHistogramParameterfv( GLenum target, GLenum pname, GLfloat *params )
  (define-function void glGetHistogramParameterfv (int int byte*))

  ;; void glGetHistogramParameteriv( GLenum target, GLenum pname, GLint *params )
  (define-function void glGetHistogramParameteriv (int int byte*))

  ;; void glMinmax( GLenum target, GLenum internalformat, GLboolean sink )
  (define-function void glMinmax (int int int))

  ;; void glResetMinmax( GLenum target )
  (define-function void glResetMinmax (int))

  ;; void glGetMinmax( GLenum target, GLboolean reset, GLenum format, GLenum types, GLvoid *values )
  (define-function void glGetMinmax (int int int int byte*))

  ;; void glGetMinmaxParameterfv( GLenum target, GLenum pname, GLfloat *params )
  (define-function void glGetMinmaxParameterfv (int int byte*))

  ;; void glGetMinmaxParameteriv( GLenum target, GLenum pname, GLint *params )
  (define-function void glGetMinmaxParameteriv (int int byte*))

  ;; void glConvolutionFilter1D( GLenum target, GLenum internalformat, GLsizei width, GLenum format, GLenum type, const GLvoid *image )
  (define-function void glConvolutionFilter1D (int int int int int byte*))

  ;; void glConvolutionFilter2D( GLenum target, GLenum internalformat, GLsizei width, GLsizei height, GLenum format, GLenum type, const GLvoid *image )
  (define-function void glConvolutionFilter2D (int int int int int int byte*))

  ;; void glConvolutionParameterf( GLenum target, GLenum pname, GLfloat params )
  (define-function void glConvolutionParameterf (int int float))

  ;; void glConvolutionParameterfv( GLenum target, GLenum pname, const GLfloat *params )
  (define-function void glConvolutionParameterfv (int int byte*))

  ;; void glConvolutionParameteri( GLenum target, GLenum pname, GLint params )
  (define-function void glConvolutionParameteri (int int int))

  ;; void glConvolutionParameteriv( GLenum target, GLenum pname, const GLint *params )
  (define-function void glConvolutionParameteriv (int int byte*))

  ;; void glCopyConvolutionFilter1D( GLenum target, GLenum internalformat, GLint x, GLint y, GLsizei width )
  (define-function void glCopyConvolutionFilter1D (int int int int int))

  ;; void glCopyConvolutionFilter2D( GLenum target, GLenum internalformat, GLint x, GLint y, GLsizei width, GLsizei height)
  (define-function void glCopyConvolutionFilter2D (int int int int int int))

  ;; void glGetConvolutionFilter( GLenum target, GLenum format, GLenum type, GLvoid *image )
  (define-function void glGetConvolutionFilter (int int int byte*))

  ;; void glGetConvolutionParameterfv( GLenum target, GLenum pname, GLfloat *params )
  (define-function void glGetConvolutionParameterfv (int int byte*))

  ;; void glGetConvolutionParameteriv( GLenum target, GLenum pname, GLint *params )
  (define-function void glGetConvolutionParameteriv (int int byte*))

  ;; void glSeparableFilter2D( GLenum target, GLenum internalformat, GLsizei width, GLsizei height, GLenum format, GLenum type, const GLvoid *row, const GLvoid *column )
  (define-function void glSeparableFilter2D (int int int int int int byte* byte*))

  ;; void glGetSeparableFilter( GLenum target, GLenum format, GLenum type, GLvoid *row, GLvoid *column, GLvoid *span )
  (define-function void glGetSeparableFilter (int int int byte* byte* byte*))

  ;; void glActiveTexture( GLenum texture )
  (define-function void glActiveTexture (int))

  ;; void glClientActiveTexture( GLenum texture )
  (define-function void glClientActiveTexture (int))

  ;; void glCompressedTexImage1D( GLenum target, GLint level, GLenum internalformat, GLsizei width, GLint border, GLsizei imageSize, const GLvoid *data )
  (define-function void glCompressedTexImage1D (int int int int int int byte*))

  ;; void glCompressedTexImage2D( GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLint border, GLsizei imageSize, const GLvoid *data )
  (define-function void glCompressedTexImage2D (int int int int int int int byte*))

  ;; void glCompressedTexImage3D( GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLsizei depth, GLint border, GLsizei imageSize, const GLvoid *data )
  (define-function void glCompressedTexImage3D (int int int int int int int int byte*))

  ;; void glCompressedTexSubImage1D( GLenum target, GLint level, GLint xoffset, GLsizei width, GLenum format, GLsizei imageSize, const GLvoid *data )
  (define-function void glCompressedTexSubImage1D (int int int int int int byte*))

  ;; void glCompressedTexSubImage2D( GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLsizei imageSize, const GLvoid *data )
  (define-function void glCompressedTexSubImage2D (int int int int int int int int byte*))

  ;; void glCompressedTexSubImage3D( GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLsizei imageSize, const GLvoid *data )
  (define-function void glCompressedTexSubImage3D (int int int int int int int int int int byte*))

  ;; void glGetCompressedTexImage( GLenum target, GLint lod, GLvoid *img )
  (define-function void glGetCompressedTexImage (int int byte*))

  ;; void glMultiTexCoord1d( GLenum target, GLdouble s )
  (define-function void glMultiTexCoord1d (int double))

  ;; void glMultiTexCoord1dv( GLenum target, const GLdouble *v )
  (define-function void glMultiTexCoord1dv (int byte*))

  ;; void glMultiTexCoord1f( GLenum target, GLfloat s )
  (define-function void glMultiTexCoord1f (int float))

  ;; void glMultiTexCoord1fv( GLenum target, const GLfloat *v )
  (define-function void glMultiTexCoord1fv (int byte*))

  ;; void glMultiTexCoord1i( GLenum target, GLint s )
  (define-function void glMultiTexCoord1i (int int))

  ;; void glMultiTexCoord1iv( GLenum target, const GLint *v )
  (define-function void glMultiTexCoord1iv (int byte*))

  ;; void glMultiTexCoord1s( GLenum target, GLshort s )
  (define-function void glMultiTexCoord1s (int int))

  ;; void glMultiTexCoord1sv( GLenum target, const GLshort *v )
  (define-function void glMultiTexCoord1sv (int byte*))

  ;; void glMultiTexCoord2d( GLenum target, GLdouble s, GLdouble t )
  (define-function void glMultiTexCoord2d (int double double))

  ;; void glMultiTexCoord2dv( GLenum target, const GLdouble *v )
  (define-function void glMultiTexCoord2dv (int byte*))

  ;; void glMultiTexCoord2f( GLenum target, GLfloat s, GLfloat t )
  (define-function void glMultiTexCoord2f (int float float))

  ;; void glMultiTexCoord2fv( GLenum target, const GLfloat *v )
  (define-function void glMultiTexCoord2fv (int byte*))

  ;; void glMultiTexCoord2i( GLenum target, GLint s, GLint t )
  (define-function void glMultiTexCoord2i (int int int))

  ;; void glMultiTexCoord2iv( GLenum target, const GLint *v )
  (define-function void glMultiTexCoord2iv (int byte*))

  ;; void glMultiTexCoord2s( GLenum target, GLshort s, GLshort t )
  (define-function void glMultiTexCoord2s (int int int))

  ;; void glMultiTexCoord2sv( GLenum target, const GLshort *v )
  (define-function void glMultiTexCoord2sv (int byte*))

  ;; void glMultiTexCoord3d( GLenum target, GLdouble s, GLdouble t, GLdouble r )
  (define-function void glMultiTexCoord3d (int double double double))

  ;; void glMultiTexCoord3dv( GLenum target, const GLdouble *v )
  (define-function void glMultiTexCoord3dv (int byte*))

  ;; void glMultiTexCoord3f( GLenum target, GLfloat s, GLfloat t, GLfloat r )
  (define-function void glMultiTexCoord3f (int float float float))

  ;; void glMultiTexCoord3fv( GLenum target, const GLfloat *v )
  (define-function void glMultiTexCoord3fv (int byte*))

  ;; void glMultiTexCoord3i( GLenum target, GLint s, GLint t, GLint r )
  (define-function void glMultiTexCoord3i (int int int int))

  ;; void glMultiTexCoord3iv( GLenum target, const GLint *v )
  (define-function void glMultiTexCoord3iv (int byte*))

  ;; void glMultiTexCoord3s( GLenum target, GLshort s, GLshort t, GLshort r )
  (define-function void glMultiTexCoord3s (int int int int))

  ;; void glMultiTexCoord3sv( GLenum target, const GLshort *v )
  (define-function void glMultiTexCoord3sv (int byte*))

  ;; void glMultiTexCoord4d( GLenum target, GLdouble s, GLdouble t, GLdouble r, GLdouble q )
  (define-function void glMultiTexCoord4d (int double double double double))

  ;; void glMultiTexCoord4dv( GLenum target, const GLdouble *v )
  (define-function void glMultiTexCoord4dv (int byte*))

  ;; void glMultiTexCoord4f( GLenum target, GLfloat s, GLfloat t, GLfloat r, GLfloat q )
  (define-function void glMultiTexCoord4f (int float float float float))

  ;; void glMultiTexCoord4fv( GLenum target, const GLfloat *v )
  (define-function void glMultiTexCoord4fv (int byte*))

  ;; void glMultiTexCoord4i( GLenum target, GLint s, GLint t, GLint r, GLint q )
  (define-function void glMultiTexCoord4i (int int int int int))

  ;; void glMultiTexCoord4iv( GLenum target, const GLint *v )
  (define-function void glMultiTexCoord4iv (int byte*))

  ;; void glMultiTexCoord4s( GLenum target, GLshort s, GLshort t, GLshort r, GLshort q )
  (define-function void glMultiTexCoord4s (int int int int int))

  ;; void glMultiTexCoord4sv( GLenum target, const GLshort *v )
  (define-function void glMultiTexCoord4sv (int byte*))

  ;; void glLoadTransposeMatrixd( const GLdouble *m ) /* void glLoadTransposeMatrixd( const GLdouble m[16] ) */
  (define-function void glLoadTransposeMatrixd (byte*))

  ;; void glLoadTransposeMatrixf( const GLfloat *m ) /* void glLoadTransposeMatrixf( const GLfloat m[16] ) */
  (define-function void glLoadTransposeMatrixf (byte*))

  ;; void glMultTransposeMatrixd( const GLdouble *m ) /* void glMultTransposeMatrixd( const GLdouble m[16] ) */
  (define-function void glMultTransposeMatrixd (byte*))

  ;; void glMultTransposeMatrixf( const GLfloat *m ) /* void glMultTransposeMatrixf( const GLfloat m[16] ) */
  (define-function void glMultTransposeMatrixf (byte*))

  ;; void glSampleCoverage( GLclampf value, GLboolean invert )
  (define-function void glSampleCoverage (float int))

  ;;;; OpenGL 1.3

  ;; void glActiveTextureARB(GLenum texture)
  (define-function void glActiveTextureARB (int))

  ;; void glClientActiveTextureARB(GLenum texture)
  (define-function void glClientActiveTextureARB (int))

  ;; void glMultiTexCoord1dARB(GLenum target, GLdouble s)
  (define-function void glMultiTexCoord1dARB (int double))

  ;; void glMultiTexCoord1dvARB(GLenum target, const GLdouble *v)
  (define-function void glMultiTexCoord1dvARB (int byte*))

  ;; void glMultiTexCoord1fARB(GLenum target, GLfloat s)
  (define-function void glMultiTexCoord1fARB (int float))

  ;; void glMultiTexCoord1fvARB(GLenum target, const GLfloat *v)
  (define-function void glMultiTexCoord1fvARB (int byte*))

  ;; void glMultiTexCoord1iARB(GLenum target, GLint s)
  (define-function void glMultiTexCoord1iARB (int int))

  ;; void glMultiTexCoord1ivARB(GLenum target, const GLint *v)
  (define-function void glMultiTexCoord1ivARB (int byte*))

  ;; void glMultiTexCoord1sARB(GLenum target, GLshort s)
  (define-function void glMultiTexCoord1sARB (int int))

  ;; void glMultiTexCoord1svARB(GLenum target, const GLshort *v)
  (define-function void glMultiTexCoord1svARB (int byte*))

  ;; void glMultiTexCoord2dARB(GLenum target, GLdouble s, GLdouble t)
  (define-function void glMultiTexCoord2dARB (int double double))

  ;; void glMultiTexCoord2dvARB(GLenum target, const GLdouble *v)
  (define-function void glMultiTexCoord2dvARB (int byte*))

  ;; void glMultiTexCoord2fARB(GLenum target, GLfloat s, GLfloat t)
  (define-function void glMultiTexCoord2fARB (int float float))

  ;; void glMultiTexCoord2fvARB(GLenum target, const GLfloat *v)
  (define-function void glMultiTexCoord2fvARB (int byte*))

  ;; void glMultiTexCoord2iARB(GLenum target, GLint s, GLint t)
  (define-function void glMultiTexCoord2iARB (int int int))

  ;; void glMultiTexCoord2ivARB(GLenum target, const GLint *v)
  (define-function void glMultiTexCoord2ivARB (int byte*))

  ;; void glMultiTexCoord2sARB(GLenum target, GLshort s, GLshort t)
  (define-function void glMultiTexCoord2sARB (int int int))

  ;; void glMultiTexCoord2svARB(GLenum target, const GLshort *v)
  (define-function void glMultiTexCoord2svARB (int byte*))

  ;; void glMultiTexCoord3dARB(GLenum target, GLdouble s, GLdouble t, GLdouble r)
  (define-function void glMultiTexCoord3dARB (int double double double))

  ;; void glMultiTexCoord3dvARB(GLenum target, const GLdouble *v)
  (define-function void glMultiTexCoord3dvARB (int byte*))

  ;; void glMultiTexCoord3fARB(GLenum target, GLfloat s, GLfloat t, GLfloat r)
  (define-function void glMultiTexCoord3fARB (int float float float))

  ;; void glMultiTexCoord3fvARB(GLenum target, const GLfloat *v)
  (define-function void glMultiTexCoord3fvARB (int byte*))

  ;; void glMultiTexCoord3iARB(GLenum target, GLint s, GLint t, GLint r)
  (define-function void glMultiTexCoord3iARB (int int int int))

  ;; void glMultiTexCoord3ivARB(GLenum target, const GLint *v)
  (define-function void glMultiTexCoord3ivARB (int byte*))

  ;; void glMultiTexCoord3sARB(GLenum target, GLshort s, GLshort t, GLshort r)
  (define-function void glMultiTexCoord3sARB (int int int int))

  ;; void glMultiTexCoord3svARB(GLenum target, const GLshort *v)
  (define-function void glMultiTexCoord3svARB (int byte*))

  ;; void glMultiTexCoord4dARB(GLenum target, GLdouble s, GLdouble t, GLdouble r, GLdouble q)
  (define-function void glMultiTexCoord4dARB (int double double double double))

  ;; void glMultiTexCoord4dvARB(GLenum target, const GLdouble *v)
  (define-function void glMultiTexCoord4dvARB (int byte*))

  ;; void glMultiTexCoord4fARB(GLenum target, GLfloat s, GLfloat t, GLfloat r, GLfloat q)
  (define-function void glMultiTexCoord4fARB (int float float float float))

  ;; void glMultiTexCoord4fvARB(GLenum target, const GLfloat *v)
  (define-function void glMultiTexCoord4fvARB (int byte*))

  ;; void glMultiTexCoord4iARB(GLenum target, GLint s, GLint t, GLint r, GLint q)
  (define-function void glMultiTexCoord4iARB (int int int int int))

  ;; void glMultiTexCoord4ivARB(GLenum target, const GLint *v)
  (define-function void glMultiTexCoord4ivARB (int byte*))

  ;; void glMultiTexCoord4sARB(GLenum target, GLshort s, GLshort t, GLshort r, GLshort q)
  (define-function void glMultiTexCoord4sARB (int int int int int))

  ;; void glMultiTexCoord4svARB(GLenum target, const GLshort *v)
  (define-function void glMultiTexCoord4svARB (int byte*))

  ) ;[end]
