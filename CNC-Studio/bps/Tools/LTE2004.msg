// LTE Kernel Messages for LTE 2004 Kernel
// Copyright (c) 2001-2004 TM-CAD Ingenieurbüro Torsten Moses
//
// [number] is the language code of LT (last 3 digits);
//          like ACLT-1:409 for English
//          like ACLT-1:407 for German  etc.
//          can be extended by developers :-)
//
// particular language can be forced by :
// FORCE_LANGUAGE=number
// example : FORCE_LANGUAGE=409 to force English langauge always

FORCE_LANGUAGE=

// ================= ENGLISH MESSAGES ==================
[409]

//----- GENERAL
GENERAL_MSG_OVERWRITE # already exists, do you want to replace it? [Yes/No] <N>
GENERAL_KEY_OVERWRITE # Yes No
GENERAL_ERR_FILEFOUND # Can't find file in search path.
GENERAL_MSG_CURWIRE   # Current wire frame density
POINT_OR_KEYWORD      # Point or option keyword required.
NO_OBJECT_FOUND       # No object found.
ERR_NUMBER_BETWEEN    # Requires an integer between %d and %d.
ERR_NUMBER_SEGMENTS   # Number of segments must be greater than 1.
ERR_NUMBER_SURFTAB    # Value must be between 2 and 256.
ERR_TRYAGAIN          # Please try again.

//------ LAYER
LAYER_ENTER_NAME      # Enter a layer name <%s>
INVALID_LAYER_NAME    # Invalid layer name.

//------ COLOR
COLOR_KEY_TC          # Truecolor
COLOR_KEY_TC_LOC      # Truecolor
COLOR_KEY_CB          # COlorbook
COLOR_KEY_CB_LOC      # COlorbook
COLOR_ERR_WRONG_NR    # Invalid color number.
COLOR_ERR_WRONG_COL   # A color number or standard color name is required.
COLOR_ERR_WRONG_RGB   # Invalid color.  Type three comma separated values from 0 to 255
COLOR_ERR_WRONG_CB    # Color book not found.
COLOR_ERR_WRONG_CN    # Color not found.
COLOR_MSG_NEWCOLOR    # New color
COLOR_MSG_RGB         # Red,Green,Blue
COLOR_MSG_COLORBOOK   # Enter Color Book name
COLOR_MSG_COLORNAME   # Enter color Name

//=============== NEW V1.9 =============

//------ COLOR NAMES
NAME_BYCOLOR          # ByColor
NAME_BYBLOCK          # ByBlock
NAME_BYLAYER          # ByLayer
COLORNAME_COL_1       # Red
COLORNAME_COL_2       # Yellow
COLORNAME_COL_3       # Green
COLORNAME_COL_4       # Cyan
COLORNAME_COL_5       # Blue
COLORNAME_COL_6       # Magenta
COLORNAME_COL_7       # White
COLORNAME_COL_BYBLOCK # ByBlock
COLORNAME_COL_BYLAYER # ByLayer
COLORNAME_COL_PREFIX  # Color
COLORNAME_COL_OTHERS  # Other ...

//------ COMMON DIALOG MESSAGES
DIALOG_BTN_APPLY         # Apply
DIALOG_BTN_OK            # OK
DIALOG_BTN_CANCEL        # Cancel
DIALOG_BTN_HELP          # Help
DIALOG_ERR_NOT_NUMERIC   # Not a numeric value !

//----- MVIEW / -VPORTS
MVIEW_CMD                 # MVIEW
VPORTS_CMD                # VPORTS
-VPORTS_CMD               # -VPORTS
MVIEW_MSG_MAIN            # Specify corner of viewport or
MVIEW_KEY_ON              # ON
MVIEW_KEY_OFF             # OFF
MVIEW_KEY_FIT             # Fit
MVIEW_KEY_SHADEPLOT       # Shadeplot
MVIEW_KEY_HIDEPLOT        # Hideplot
MVIEW_KEY_LOCK            # Lock
MVIEW_KEY_OBJECT          # Object
MVIEW_KEY_POLYGONAL       # Polygonal
MVIEW_KEY_RESTORE         # Restore
MVIEW_KEY_HORIZONTAL      # Horizontal
MVIEW_KEY_VERTICAL        # Vertical
MVIEW_KEY_ABOVE           # Above
MVIEW_KEY_BELOW           # Below
MVIEW_KEY_LEFT            # Left
MVIEW_KEY_RIGHT           # Right
MVIEW_KEY_SHADE_ASIS      # As displayed
MVIEW_KEY_SHADE_WIRE      # Wireframe
MVIEW_KEY_SHADE_HIDDEN    # Hidden
MVIEW_KEY_SHADE_RENDERED  # Rendered
MVIEW_MSG_PSPACE          # Switching to Paper space.
MVIEW_MSG_MSPACE          # Switching to Model space.
MVIEW_MSG_OBJECT          # Select object to clip viewport
MVIEW_MSG_SHADEPLOT       # Shade plot ?
MVIEW_MSG_HIDEPLOT        # Hidden line removal for plotting
MVIEW_MSG_LOCKVPORT       # Viewport View Locking
MVIEW_MSG_FIRSTCORNER     # Specify first corner or
MVIEW_MSG_OTHERCORNER     # Specify opposite corner
MVIEW_MSG_ARRANGEMENT     # Enter viewport arrangement
MVIEW_MSG_RESTORE         # Enter viewport configuration name or [?]
MVIEW_MSG_RESTORENAMES    # Enter name(s) of viewport configuration(s) to list <*>
MVIEW_MSG_CONFIGURATION   # Configuration
MVIEW_ERR_NOOBJECTFOUND   # No object found.
MVIEW_ERR_INVALIDOBJECT   # Invalid viewport clip object.
MVIEW_ERR_OBJECTNOTCLOSED # Clip object must be closed.
MVIEW_ERR_OBJECTNOTVPORT  # object(s) not viewport(s).
MVIEW_ERR_NOVPORTCFGFOUND # Cannot find viewport configuration

//----- TrueColor Dialog
DLG_SELECTCOLOR_TITLE              # Select Color
DLG_SELECTCOLOR_PAGE_ACI_TITLE     # Index Color
DLG_SELECTCOLOR_PAGE_ACI_MSG       # AutoCAD Color Index (ACI)
DLG_SELECTCOLOR_PAGE_ACI_MSG1      # Color Index
DLG_SELECTCOLOR_PAGE_ACI_MSG2      # Red, Green, Blue
DLG_SELECTCOLOR_PAGE_RGB_TITLE     # True Color
DLG_SELECTCOLOR_PAGE_RGB_SPACE     # Space
DLG_SELECTCOLOR_PAGE_RGB_WEB       # Web
DLG_SELECTCOLOR_PAGE_CBK_TITLE     # Color Books
DLG_SELECTCOLOR_PAGE_CBK_MSG       # Color book :
DLG_SELECTCOLOR_PAGE_CBK_SORT      # Sort color names
DLG_SELECTCOLOR_PAGE_CBK_FOLDER    # Color Book Location
DLG_SELECTCOLOR_PAGE_CBK_SELECT    # Specify Color Books location
DLG_SELECTCOLOR_PAGE_CBK_MSG1      # Please Note
DLG_SELECTCOLOR_PAGE_CBK_MSG2      # You must close and restart AutoCAD LT to use new folder !
DLG_SELECTCOLOR_PAGE_CBK_CUSTOM    # How to create ColorBooks ?
DLG_SELECTCOLOR_PAGE_CBK_ACBHELP   # Custom-ACB.English.rtf
DLG_SELECTCOLOR_PAGE_CBK_NOACBRTF  # Documentation file in ACB Customization not found !
DLG_SELECTCOLOR_PAGE_CBK_NOWORDPAD # The "WordPad.exe" editor could not be found !

//----- MLINE STYLE
MLSTYLE_CMD                 # MLSTYLE
MLSTYLE_DLG_TITLE           # Multiline Styles
MLSTYLE_DLG_MSG_CURR        # Current
MLSTYLE_DLG_MSG_DESC        # Description
MLSTYLE_DLG_BTN_LOAD        # Load
MLSTYLE_DLG_BTN_SAVE        # Save
MLSTYLE_DLG_BTN_ADD         # Add
MLSTYLE_DLG_BTN_RENAME      # Rename
MLSTYLE_DLG_BTN_DELETE      # Delete
MLSTYLE_DLG_PAGE_ELEMENTS   # Element Properies
MLSTYLE_DLG_MSG_OFFSET      # Offset
MLSTYLE_DLG_MSG_COLOR       # Color
MLSTYLE_DLG_MSG_LTYPE       # Linetype
MLSTYLE_DLG_PAGE_MLPROPS    # Multiline Properies
MLSTYLE_DLG_CHK_JOINTS      # Display Joints
MLSTYLE_DLG_CHK_FILL        # Fill
MLSTYLE_DLG_FRM_CAPS        # Caps
MLSTYLE_DLG_MSG_LINE        # Line
MLSTYLE_DLG_MSG_OARC        # Outer Arc
MLSTYLE_DLG_MSG_IARC        # Inner Arcs
MLSTYLE_DLG_MSG_ANGLE       # Angle
MLSTYLE_DLG_MSG_ATSTART     # at Start
MLSTYLE_DLG_MSG_ATEND       # at End
MLSTYLE_DLG_MSG_ENTITIES    # Number of MLines using this style
MLSTYLE_DLG_MSG_DELETE_1    # Delete Element ?
MLSTYLE_DLG_MSG_DELETE_2    # Do you want to delete this selected element ?
MLSTYLE_DLG_SAVEFILE_TITLE  # Save Current Multiline Style
MLSTYLE_DLG_LOADFILE_TITLE  # Load Multiline Styles
MLSTYLE_DLG_LOADFILE_FILE   # File ...
MLSTYLE_DLG_LOADFILE_FMSG   # Select Multiline Style File

//----- MLINE
MLINE_CMD          # MLINE
MLINE_MSG_CURR     # Current settings
MLINE_MSG_MAIN     # Specify start point or [Justification/Scale/STyle]
MLINE_KEY_MAIN     # Justification Scale STyle
MLINE_KEY_JUST     # Justification
MLINE_KEY_SCALE    # Scale
MLINE_KEY_STYLE    # STyle
MLINE_JUST_TOP     # Top
MLINE_JUST_ZERO    # Zero
MLINE_JUST_BOTTOM  # Bottom
MLINE_MSG_JUST     # Enter justification type [Top/Zero/Bottom]
MLINE_MSG_SCALE    # Enter MLine scale
MLINE_MSG_STYLE    # Enter MLine style name or [?]
MLINE_MSG_STLIST   # Name                               Description
MLINE_MSG_FILEDLG  # Load MultiLine Style from file
MLINE_ERR_NOTFOUND # Multiline style <%s> not found in file
MLINE_MSG_PTNEXT1  # Specify next point
MLINE_MSG_PTNEXT2  # Specify next point or
MLINE_KEY_UNDO     # Undo
MLINE_KEY_CLOSE    # Close

//----- -REFEDIT
REFEDITCMDLINE_CMD       # -REFEDIT
REFEDITCMDLINE_MSG_MAIN  # Select nesting level [Ok/Next] <Next>
REFEDITCMDLINE_KWD_MAIN  # Ok Next
REFEDITCMDLINE_MSG_ATTS  # Display attribute definitions [Yes/No] <No>
REFEDITCMDLINE_KWD_ATTS  # Yes No
REFEDITCMDLINE_MSG_MODE  # Enter object selection method [All/Nested] <All>
REFEDITCMDLINE_KWD_MODE  # All Nested

//----- REFEDIT
REFEDIT_CMD              # REFEDIT
REFEDIT_MSG_SELECT       # Select XRef/Block reference
REFEDIT_MSG_NOTICE       # Use REFCLOSE or the RefEdit toolbar to end reference editing session.
REFEDIT_MSG_RELOAD       # XRef is outdated - XRef drawing has been reloaded.
REFEDIT_MSG_NRITEMSADDED # objects were added to the workset.
REFEDIT_ERR_INPROGRESS   # ** Command not allowed, XRef/Block already checked out for editing **
REFEDIT_ERR_NOXEDIT      # This XRef cannot be in-place edited : XEDIT=0 set inside this XRef !
REFEDIT_ERR_DWGLOCKED1   # This XRef cannot be checked out for editing.
REFEDIT_ERR_DWGLOCKED2   # A file lock already exists on the drawing.
REFEDIT_ERR_NOREFERENCE  # Object is not a reference.
REFEDIT_ERR_NOSELECTION  # Reference not found.
REFEDIT_ERR_NOENTITIES   # No entities selected for working set.
REFEDIT_ERR_NO_SAVE      # command not allowed during reference editing

REFEDIT_ERR_HOSTOBJECT_1 # The object(s) you selected belong(s) to the host drawing !
REFEDIT_ERR_HOSTOBJECT_2 # Please be careful when adding host objects to working set !

//----- REFEDIT DIALOG
REFEDIT_DLG_TITLE        # Reference Edit
REFEDIT_DLG_PAGE_REFS    # Identify Reference
REFEDIT_DLG_REFNAME      # Reference Name
REFEDIT_DLG_PREVIEW      # Preview
REFEDIT_DLG_PATH         # Path
REFEDIT_DLG_AUTO         # Automatically select all nested objects
REFEDIT_DLG_MANU         # Prompt to manually select nested objects
REFEDIT_DLG_TIP_AUTO     # This will automatically include all objects of selected reference into working set for in-place editing.
REFEDIT_DLG_TIP_MANU     # This will prompt you to manually select those objects to be included into working set for in-place editing.
REFEDIT_DLG_PAGE_OPTS    # Settings
REFEDIT_DLG_UNIQUE       # Create unique layer, style and block name
REFEDIT_DLG_ATTRIBUTES   # Display attribute definitions for editing
REFEDIT_DLG_LOCK         # Lock objects not in working set
REFEDIT_DLG_HIDESELECT   # Hide all objects not in working set during selection
REFEDIT_DLG_HIDEEDIT     # Hide all objects not in working set during reference edit
REFEDIT_DLG_FILTER       # Use Objecttype Filter
REFEDIT_DLG_SETFILTER    # Define Filter ...
REFEDIT_DLG_TIP_UNIQUE   # Controls wether layer, blocks, styles and other named table objects extracted from reference will be named uniquely during in-place edit (like BINDTYPE)
REFEDIT_DLG_TIP_ATTRIBS  # Controls wether all non-constant attribute definitions in block references are extracted and displayed during reference editing.
REFEDIT_DLG_TIP_LOCK     # Locks all objects not in working set. This will prevent you from accidentally selecting and editing objects in the host drawing while in reference editing state.
REFEDIT_DLG_TIP_HIDESEL  # Hides all objects in the host drawing (except selected reference) during selection of objects to be included in working set. This will also prevent you from selecting unwanted host drawing objects.
REFEDIT_DLG_TIP_HIDEEDT  # Hides all objects in the host drawing (except selected objects) during reference edit. Provides more clearness on screen - can be changed dynamically from 'Reference Edit' toolbar.
REFEDIT_DLG_TIP_FILTER   # Allows to specify particular types of objects to be included into working set for reference edit.
REFEDIT_DLG_TIP_SETFILTER# Define your filter conditions ...

REFEDIT_DLG_FILTER_TITLE # Object Filter
REFEDIT_FLT_BTN_ALL      # All
REFEDIT_FLT_BTN_NONE     # None
REFEDIT_FLT_POINT        # Point
REFEDIT_FLT_LINE         # Line
REFEDIT_FLT_MLINE        # MLine
REFEDIT_FLT_POLYLINE     # Polyline
REFEDIT_FLT_SPLINE       # Spline
REFEDIT_FLT_ARC          # Arc
REFEDIT_FLT_CIRCLE       # Circle
REFEDIT_FLT_ELLIPSE      # Ellipse
REFEDIT_FLT_3DFACE       # 3D Face
REFEDIT_FLT_SOLID        # Solid
REFEDIT_FLT_INSERT       # Insert/Xref
REFEDIT_FLT_HATCH        # Hatch
REFEDIT_FLT_IMAGE        # Image
REFEDIT_FLT_SHAPE        # Symbol
REFEDIT_FLT_TRACE        # Trace
REFEDIT_FLT_VIEWPORT     # Viewport
REFEDIT_FLT_TEXT         # Text
REFEDIT_FLT_MTEXT        # MText
REFEDIT_FLT_LEADER       # Leader
REFEDIT_FLT_DIMENSION    # Dimension
REFEDIT_FLT_REGION       # Region
REFEDIT_FLT_3DSOLID      # 3D Solid
REFEDIT_FLT_MPOLYGON     # MPolygon
REFEDIT_FLT_OTHERS       # Others

//----- REFEDIT TOOLBAR
REFEDIT_TB_TITLE         # Reference Edit
REFEDIT_TB_VISIBLE       # Display or Hide objects not in working set
REFEDIT_TB_TIP_NAME      # Shows the active XRef/Block for current reference editing session
REFEDIT_TB_TIP_EDIT      # Inplace Edit XRef or Block
REFEDIT_TB_TIP_ADD       # Add objects to working set
REFEDIT_TB_TIP_REM       # Remove objects from working set
REFEDIT_TB_TIP_DISCARD   # Discard changes to XRef or Block reference
REFEDIT_TB_TIP_SAVE      # Save back changes to XRef or Block reference
REFEDIT_TB_TIP_XFADE     # Change XFadeCtl value between 0 and 90 percent

//----- REFSET
REFSET_CMD               # REFSET
REFSET_MSG_INFO          # Transfer objects between the Refedit working set and host drawing.
REFSET_MSG_ADDREM        # Enter an option [Add/Remove] <Add>
REFSET_KWD_ADDREM        # Add Remove
REFSET_MSG_NRITEMSADD    # objects were added to the workset.
REFSET_MSG_NRITEMSREM    # objects were removed from the workset.
REFSET_ERR_NOREFEDIT     # ** Command not allowed unless a XRef/Block is checked out with REFEDIT **

//----- REFCLOSE
REFCLOSE_CMD             # REFCLOSE
REFCLOSE_MSG_CLOSE       # Enter option [Save/Discard reference changes] <Save>
REFCLOSE_KWD_CLOSE       # Save Discard
REFCLOSE_MSG_SAVE_1      # All reference edits will be saved.
REFCLOSE_MSG_SAVE_2      # - To SAVE reference changes, click <OK>.
REFCLOSE_MSG_SAVE_3      # - To CANCEL the command, click <CANCEL>.
REFCLOSE_MSG_DISCARD_1   # All reference edits will be discarded.
REFCLOSE_MSG_DISCARD_2   # - To DISCARD reference changes, click <OK>.
REFCLOSE_MSG_DISCARD_3   # - To CANCEL the command, click <CANCEL>.

REFCLOSE_ERR_NOREFEDIT   # ** Command not allowed unless a XRef/Block is checked out with REFEDIT **


//----- SCRIPT
SCRIPT_CMD             # SCRIPT
SCRIPT_MSG_SELECT      # Enter script file name
SCRIPT_DLG_SELECT      # Select Script File
SCRIPT_DLG_FILETYPE    # Script Files (*.scr)
SCRIPT_ERR_NOFILEFOUND # Cannot find the file

//------ PROFILE PAGE OPTIONS DIALOG
PROFILE_PAGE_MSG_PROFILES  # Profiles
PROFILE_PAGE_MSG_CURRENT   # Current Profile :
PROFILE_PAGE_BTN_CURRENT   # Set Current
PROFILE_PAGE_BTN_NEW       # New
PROFILE_PAGE_BTN_COPY      # Copy
PROFILE_PAGE_BTN_RENAME    # Rename
PROFILE_PAGE_BTN_DELETE    # Delete
PROFILE_PAGE_BTN_EXPORT    # Export
PROFILE_PAGE_BTN_IMPORT    # Import
PROFILE_PAGE_BTN_RESET     # Reset
PROFILE_PAGE_MSG_DELETE    # Are you sure you want to delete this profile ?
PROFILE_PAGE_MSG_NODELETE  # Cannot delete a profile that is in use !
PROFILE_PAGE_MSG_RESET     # Are you sure you want to reset this profile ?

PROFILE_PAGE_TITLE_ADD     # Add Profile
PROFILE_PAGE_TITLE_COPY    # Copy Profile
PROFILE_PAGE_TITLE_RENAME  # Change Profile
PROFILE_PAGE_TITLE_EXPORT  # Export Profile
PROFILE_PAGE_TITLE_IMPORT  # Import Profile
PROFILE_PAGE_MSG_NAME      # Profile Name
PROFILE_PAGE_MSG_DESC      # Description
PROFILE_PAGE_MSG_PATH      # Include Path information
PROFILE_PAGE_MSG_FILETYPE  # Profiles (*.arg)|arg

PROFILE_DLG_EDIT_MSG_NAME  # Profile name
PROFILE_DLG_EDIT_MSG_DESC  # Description
PROFILE_DLG_EDIT_BTN_PATH  # Include path information
PROFILE_DLG_EDIT_BTN_OK    # Apply && Close
PROFILE_DLG_EDIT_BTN_QUIT  # Cancel

//------ 3D ORBIT CONTEXT MENU
3DORBIT_MENU_EXIT        # Exit
3DORBIT_MENU_SHADINGMODE # Shading Mode
3DORBIT_MENU_PROJECTION  # Projection
3DORBIT_MENU_CLIPPING    # Clipping
3DORBIT_MENU_OTHERS      # Others
3DORBIT_MENU_PAN         # Pan
3DORBIT_MENU_ZOOM        # Zoom
3DORBIT_MENU_ORBIT       # Orbit
3DORBIT_MENU_CORBIT      # Cont. Orbit
3DORBIT_MENU_DISTANCE    # 3D-Distance
3DORBIT_MENU_SWIVEL      # 3D-Swivel
3DORBIT_MENU_FRONTCLIP   # Front Clipping On
3DORBIT_MENU_BACKCLIP    # Back Clipping On
3DORBIT_MENU_CLIPDLG     # Adjust Clipping Planes
3DORBIT_MENU_PARALLEL    # Parallel
3DORBIT_MENU_PERSPECTIVE # Perspective
3DORBIT_MENU_REFERENCEPT # Pick Center Point
3DORBIT_MENU_GETDWGVIEW  # Get View from DWG
3DORBIT_MENU_SETDWGVIEW  # Set View to DWG
3DORBIT_MENU_3DCFG       # 3D Configuration

//------ 3D ORBIT
3DORBIT_CMD              # 3DORBIT
3DORBIT_MSG_1            # Press ESC or ENTER to exit, or right-click to display shortcut-menu.
3DORBIT_MSG_2            # Left+Mouse = ORBIT, Shift-Left+Mouse = PAN, Ctrl-Left+Mouse = ZOOM

//------ 3D CONT ORBIT
3DCORBIT_CMD             # 3DCORBIT
3DCORBIT_MSG_1           # Press ESC or ENTER to exit, or right-click to display shortcut-menu.
3DCORBIT_MSG_2           # Left+Mouse = CONT.ORBIT, Shift-Left+Mouse = PAN, Ctrl-Left+Mouse = ZOOM

//------ 3D ORBIT CENTER
3DORBITCTR_CMD           # 3DORBITCTR
3DORBITCTR_MSG_0         # Specify orbit center point :
3DORBITCTR_MSG_1         # Press ESC or ENTER to exit, or right-click to display shortcut-menu.
3DORBITCTR_MSG_2         # Left+Mouse = CONT.ORBIT, Shift-Left+Mouse = PAN, Ctrl-Left+Mouse = ZOOM

//------ 3D DISTANCE
3DDISTANCE_CMD           # 3DDISTANCE
3DDISTANCE_MSG_1         # Press ESC or ENTER to exit, or right-click to display shortcut-menu.
3DDISTANCE_MSG_2         # Left+Mouse = 3D-DISTANCE, Shift-Left+Mouse = PAN, Ctrl-Left+Mouse = ZOOM

//------ 3D SWIVEL
3DSWIVEL_CMD             # 3DSWIVEL
3DSWIVEL_MSG_1           # Press ESC or ENTER to exit, or right-click to display shortcut-menu.
3DSWIVEL_MSG_2           # Left+Mouse = 3D-SWIVEL, Shift-Left+Mouse = PAN, Ctrl-Left+Mouse = ZOOM

//------ 3D CAMERA
3DCAMERA_CMD             # CAMERA
3DCAMERA_MSG_CAMERA      # Specify new camera position
3DCAMERA_MSG_TARGET      # Specify new camera target

//------ 3D CLIP
3DCLIP_CMD               # 3DCLIP
3DCLIP_DIALOG_TITLE      # Adjust Clipping Planes
3DCLIP_MSG               # Press ESC or ENTER to exit, or right-click to display shortcut-menu.
3DCLIP_BTN_CLOSE         # Close
3DCLIP_TIP_ADJUSTFRONT   # Adjust Front clipping plane
3DCLIP_TIP_ADJUSTBACK    # Adjust Back clipping plane
3DCLIP_TIP_ADJUSTSLICE   # Adjust Front & Back clipping planes as slice
3DCLIP_TIP_FRONTONOFF    # Activate Front Clipping (On/Off)
3DCLIP_TIP_BACKONOFF     # Activate  Back Clipping (On/Off)
3DCLIP_TIP_EDITFRONT     # Define Front Clipping Plane by manual edit
3DCLIP_TIP_EDITBACK      # Define Back Clipping Plane by manual edit
3DCLIP_TIP_VIEWAREA_PAN  # Mouse within View : || Left = set Front/Back Clip || Shift-Left = Pan || CTRL-Left = Zoom

//------ 3D ORBIT VIEW
3DORBITVIEW_CMD               # 3DORBITVIEW
3DORBITVIEW_MSG_PICKREF       # Please pick new reference point :
3DORBITVIEW_MODE_2D           # 2D Optimized
3DORBITVIEW_MODE_3D           # 3D Wire Frame
3DORBITVIEW_MODE_HIDDEN       # Hidden Line
3DORBITVIEW_MODE_FLAT         # Flat
3DORBITVIEW_MODE_GOURAUD      # Gouraud
3DORBITVIEW_MODE_FLATX        # Flat+Edges
3DORBITVIEW_MODE_GOURAUDX     # Gouraud+Edges
3DORBITVIEW_BTN_CLIPDLG       # 3D-Clip ...
3DORBITVIEW_BTN_EXTRAS>       # Layer >>>
3DORBITVIEW_BTN_EXTRAS<       # Layer <<<
3DORBITVIEW_BTN_SMART         # Layer Preview
3DORBITVIEW_BTN_ALL           # All
3DORBITVIEW_BTN_NONE          # None
3DORBITVIEW_BTN_SEL           # Check
3DORBITVIEW_BTN_UNSEL         # Uncheck
3DORBITVIEW_TIP_VIEWMODE      # Display mode for 3D-View
3DORBITVIEW_TIP_ANGLE_XY      # horizontal view angle in XY-plane
3DORBITVIEW_TIP_ANGLE_Z       # vertical view angle above XY-plane
3DORBITVIEW_TIP_GETDWG_VIEW   # Take drawing's view into 3d-Orbit
3DORBITVIEW_TIP_SETDWG_VIEW   # Take 3d-Orbit view into drawing's view
3DORBITVIEW_TIP_GET_REFPT     # Pick reference point inside drawing
3DORBITVIEW_TIP_CFG           # Opens 3D Graphics System Configuration
3DORBITVIEW_TIP_RELOAD        # Re-Load 3D-Orbit-Viewer model from drawing
3DORBITVIEW_TIP_SLIDER_HOR    # Set horizontal view angle
3DORBITVIEW_TIP_SLIDER_VER    # Set vertical view angle
3DORBITVIEW_TIP_SLIDER_ZOOM   # Set relative zoom scale (0.01 - 100)
3DORBITVIEW_TIP_EXTRAS        # Shows advanced features window
3DORBITVIEW_TIP_LAYERS        # Select layers to show in 3D Orbit
3DORBITVIEW_TIP_SMARTLAYERS   # If 'Layer Preview' is active, 3D Orbit will show marked Layer(s) only; otherwise, all checked Layers will be shown
3DORBITVIEW_TIP_LOADLAYERS    # Re-Load layers from drawing (update layer list)
3DORBITVIEW_TIP_ALL           # Select all layers
3DORBITVIEW_TIP_NONE          # Deselect all layers
3DORBITVIEW_TIP_SEL           # Checks all selected Layers
3DORBITVIEW_TIP_UNSEL         # Unchecks all selected Layers
3DORBITVIEW_TIP_MODE_ORBIT    # Mouse Mode : Orbit
3DORBITVIEW_TIP_MODE_PAN      # Mouse Mode : Pan
3DORBITVIEW_TIP_MODE_ZOOM     # Mouse Mode : Zoom
3DORBITVIEW_TIP_MODE_DIST     # Mouse Mode : Distance
3DORBITVIEW_TIP_MODE_SWIV     # Mouse Mode : Swivel
3DORBITVIEW_TIP_MODE_CORBIT   # Continuous Orbit : On/Off
3DORBITVIEW_TIP_MODE_PERSP    # Projection : Perspective=On/Parallel=Off
3DORBITVIEW_TIP_FRONTCLIP     # Set Front-Clip On/Off
3DORBITVIEW_TIP_BACKCLIP      # Set Back-Clip On/Off
3DORBITVIEW_TIP_CLIPDLG       # Define Front- & Back-Clipping planes
3DORBITVIEW_TIP_MODE_SAVEDISK # Save rendered image as file

3DORBITVIEW_TIP_VIEWAREA_ORB  # Mouse within 3D-View : || Left = Orbit || Shift-Left = Pan  || Ctrl-Left = Zoom  || Right = Context-Menu
3DORBITVIEW_TIP_VIEWAREA_PAN  # Mouse within 3D-View : || Left = Pan   || Shift-Left = Zoom || Ctrl-Left = Orbit || Right = Context-Menu
3DORBITVIEW_TIP_VIEWAREA_ZOOM # Mouse within 3D-View : || Left = Zoom  || Shift-Left = Pan  || Ctrl-Left = Orbit || Right = Context-Menu
3DORBITVIEW_TIP_VIEWAREA_DIST # Mouse within 3D-View : || Left = Distance || Shift-Left = Pan  || Ctrl-Left = Zoom || Right = Context-Menu
3DORBITVIEW_TIP_VIEWAREA_SWIV # Mouse within 3D-View : || Left = Swivel || Shift-Left = Pan  || Ctrl-Left = Zoom || Right = Context-Menu
3DORBITVIEW_TIP_VIEWAREA_CORB # Mouse within 3D-View : continuing Orbit

//------ TEXT & ATTRIBUTE
TEXTATT_JUST_BASE_LEFT   # Left
TEXTATT_JUST_BASE_ALIGN  # Align
TEXTATT_JUST_BASE_FIT    # Fit
TEXTATT_JUST_BASE_CENTER # Center
TEXTATT_JUST_BASE_MIDDLE # Middle
TEXTATT_JUST_BASE_RIGHT  # Right
TEXTATT_JUST_TOP_LEFT    # Top Left
TEXTATT_JUST_TOP_CENTER  # Top Center
TEXTATT_JUST_TOP_RIGHT   # Top Right
TEXTATT_JUST_MID_LEFT    # Middle Left
TEXTATT_JUST_MID_CENTER  # Middle Center
TEXTATT_JUST_MID_RIGHT   # Middle Right
TEXTATT_JUST_BOT_LEFT    # Bottom Left
TEXTATT_JUST_BOT_CENTER  # Bottom Center
TEXTATT_JUST_BOT_RIGHT   # Bottom Right

//------ EATTEDIT DIALOG
EATTEDIT_CMD                # EATTEDIT
EATTEDIT_MSG_SELBLOCK       # Select a block
EATTEDIT_ERR_SELBLOCK       # Error selecting entity.
EATTEDIT_ERR_NOBLATTS       # This drawing contains no attributed blocks.
EATTEDIT_ERR_NOBLOCK        # The selected entity is not a block.
EATTEDIT_ERR_NOATTS         # The selected block has no editable attributes.
//------ MAIN DIALOG PAGE
EATTEDIT_DLG_TITLE          # Enhanced Atrribute Editor
EATTEDIT_DLG_BLOCK          # Block
EATTEDIT_DLG_TAG            # Tag
EATTEDIT_DLG_SELECTBLOCK    # Select Block
//------ PAGE : ATTRIBUTE
ATTRIBUTE_PAGE_ATT_TITLE    # Attribute
ATTRIBUTE_PAGE_ATT_TAG      # Tag
ATTRIBUTE_PAGE_ATT_PROMPT   # Prompt
ATTRIBUTE_PAGE_ATT_VALUE    # Value
ATTRIBUTE_PAGE_ATT_DEFAULT  # Default
ATTRIBUTE_PAGE_ATT_FRAME_1  # Mode
ATTRIBUTE_PAGE_ATT_FRAME_2  # Data
ATTRIBUTE_PAGE_ATT_INVIS    # Invisible
ATTRIBUTE_PAGE_ATT_CONSTANT # Constant
ATTRIBUTE_PAGE_ATT_VERIFY   # Verify
ATTRIBUTE_PAGE_ATT_PRESET   # Preset
//------ PAGE : TEXT
ATTRIBUTE_PAGE_TXT_TITLE    # Text Options
ATTRIBUTE_PAGE_TXT_STYLE    # Text Style
ATTRIBUTE_PAGE_TXT_JUSTIFY  # Justification
ATTRIBUTE_PAGE_TXT_BACKWARD # Backwards
ATTRIBUTE_PAGE_TXT_UPSIDE   # Upside down
ATTRIBUTE_PAGE_TXT_HEIGHT   # Height
ATTRIBUTE_PAGE_TXT_WIDTH    # Width Factor
ATTRIBUTE_PAGE_TXT_ROTATION # Rotation
ATTRIBUTE_PAGE_TXT_ANGLE    # Oblique Angle
//------ PAGE : PROPERTIES
ATTRIBUTE_PAGE_PRP_TITLE    # Properties
ATTRIBUTE_PAGE_PRP_LAYER    # Layer
ATTRIBUTE_PAGE_PRP_LTYPE    # Linetype
ATTRIBUTE_PAGE_PRP_COLOR    # Color
ATTRIBUTE_PAGE_PRP_LWEIGHT  # Lineweight
ATTRIBUTE_PAGE_PRP_PSTYLE   # Plot Style

//------ BATTMAN DIALOG
BATTMAN_CMD                 # BATTMAN
//------ MAIN DIALOG PAGE
BATTMAN_DLG_TITLE           # Block Atrribute Manager
BATTMAN_DLG_SELECTBLOCK     # Select Block
BATTMAN_DLG_BLOCK           # Block :
BATTMAN_DLG_SYNCALL         # Sync
BATTMAN_DLG_SYNCSEL         # Sync Sel
BATTMAN_DLG_MOVEUP          # Move Up
BATTMAN_DLG_MOVEDOWN        # Move Down
BATTMAN_DLG_EDIT            # Edit
BATTMAN_DLG_REMOVE          # Remove
BATTMAN_DLG_SETTINGS        # Settings ...
BATTMAN_MSG_SELECTINSERTS   # Select Blockreferences to synchronize
BATTMAN_MSG_FOUND           # Found in drawing
BATTMAN_MSG_FOUNDINMODEL    # Found in model space
BATTMAN_MSG_FOUNDINLAYOUT   # Found in current layout
BATTMAN_MSG_BLOCKMODIFIED_1 # The attributes are changed for current block
BATTMAN_MSG_BLOCKMODIFIED_2 # Do you want to save these changes ?
BATTMAN_KEY_TAG             # Tag
BATTMAN_KEY_PROMPT          # Prompt
BATTMAN_KEY_DEFAULT         # Default
BATTMAN_KEY_MODES           # Modes
BATTMAN_KEY_MODES_INVISIBLE # Invisible
BATTMAN_KEY_MODES_CONSTANT  # Constant
BATTMAN_KEY_MODES_PRESET    # Preset
BATTMAN_KEY_MODES_VERIFY    # Verify
BATTMAN_KEY_STYLE           # Style
BATTMAN_KEY_JUSTIFY         # Justification
BATTMAN_KEY_HEIGHT          # Height
BATTMAN_KEY_ROTATION        # Rotation
BATTMAN_KEY_WIDTHFACTOR     # Width Factor
BATTMAN_KEY_OBLIQUE         # Oblique Angle
BATTMAN_KEY_LAYER           # Layer
BATTMAN_KEY_LINETYPE        # Linetype
BATTMAN_KEY_COLOR           # Color
BATTMAN_KEY_LINEWEIGHT      # Lineweight
BATTMAN_KEY_PLOTSTYLE       # Plotstyle
BATTMAN_ERR_TAGMISSING      # A tag name must be provided.
//------ BATTMAN SETTINGS DIALOG
BATTMAN_SETTINGS_DLG_TITLE  # Settings
BATTMAN_SETTINGS_DLG_FRAME  # Display in List
BATTMAN_SETTINGS_DLG_ALL    # Select All
BATTMAN_SETTINGS_DLG_NONE   # Clear All
BATTMAN_SETTINGS_DLG_EMPHAS # Emphasize duplicate tags
BATTMAN_SETTINGS_DLG_APPLY  # Apply changes to existing references
//------ BATTMAN ATTRIBUT EDIT DIALOG
BATTMAN_ATTRIB_DLG_TITLE    # Edit Attribute
BATTMAN_ATTRIB_DLG_BLOCK    # Block
BATTMAN_ATTRIB_DLG_ATTRIB   # Attribute
BATTMAN_ATTRIB_DLG_AUTOAPP  # Auto preview changes

//------ ATTSYNC
ATTSYNC_CMD           # ATTSYNC
ATTSYNC_MSG           # Enter an option [?/Name/Select] <Select>
ATTSYNC_KEY           # ? Name Select
ATTSYNC_SELECT        # Select a block
ATTSYNC_LIST          # Attributed blocks
ATTSYNC_NAME          # Enter name of block to sync or [?]
ATTSYNC_DOSYNC        # ATTSYNC block %s ? [Yes/No] <Yes>:
ATTSYNC_KEY_YESNO     # Yes No
ATTSYNC_COMPLETE      # ATTSYNC complete.
ATTSYNC_ERR_SELBLOCK  # Error selecting entity.
ATTSYNC_ERR_NOBLOCK   # The selected entity is not a block.
ATTSYNC_ERR_NOBLATTS  # This drawing contains no attributed blocks.
ATTSYNC_ERR_NOATTS    # The selected block has no editable attributes.
ATTSYNC_ERR_WRONGNAME # No block defined with given name !

//------ LAYERPMODE
LAYERPMODE_CMD        # LAYERPMODE
LAYERPMODE_MSG        # Enter LAYERP mode [ON/OFF]
LAYERPMODE_KWD_ON     # ON
LAYERPMODE_KWD_OFF    # OFF

//------ LAYERP
LAYERP_CMD            # LAYERP
LAYERP_MSG_NORM       # Restored previous layer status.
LAYERP_MSG_NOTE       # NOTE: Viewport Freeze/Thaw changes were applied to the original viewport. Some changes may not be apparent in the current viewport.
LAYERP_MSG_FAIL       # * No previous layer status *

//======================================

//------ SHADEMODE
SHADEMODE_CMD              # SHADEMODE
SHADEMODE_MSG_MODE         # Current mode
SHADEMODE_MSG_2D           # 2d wireframe
SHADEMODE_MSG_3D           # 3d wireframe
SHADEMODE_MSG_HIDDEN       # Hidden
SHADEMODE_MSG_FLAT         # Flat
SHADEMODE_MSG_GOURAUD      # Gouraud
SHADEMODE_MSG_FLAT+        # Flat+Edges
SHADEMODE_MSG_GOURAUD+     # Gouraud+Edges
SHADEMODE_MSG_REQUEST_MODE # Enter option [2D wireframe/3D wireframe/Hidden/Flat/Gouraud/fLat+edges/gOuraud+edges]
SHADEMODE_KWD_REQUEST_MODE # 2d 3d Hidden Flat Gouraud fLat+edges gOuraud+edges

//------ IMAGE
IMAGE_CMD                  # IMAGE
IMAGE_CMD_DLG_TITLE        # Image Manager
IMAGE_CMD_DLG_BTN_OK       # OK
IMAGE_CMD_DLG_BTN_CANCEL   # Cancel
IMAGE_CMD_DLG_BTN_HELP     # Help
IMAGE_CMD_DLG_BTN_ATTACH   # Attach
IMAGE_CMD_DLG_BTN_DETACH   # Detach
IMAGE_CMD_DLG_BTN_UNLOAD   # Unload
IMAGE_CMD_DLG_BTN_RELOAD   # Reload
IMAGE_CMD_DLG_BTN_BROWSE   # Browse
IMAGE_CMD_DLG_BTN_SAVEPATH # Save Path
IMAGE_CMD_DLG_BTN_DETAILS+ # Details >>
IMAGE_CMD_DLG_BTN_DETAILS- # Details <<
IMAGE_CMD_DLG_COL_NAME     # Image Name
IMAGE_CMD_DLG_COL_COUNT    # Number
IMAGE_CMD_DLG_COL_STATUS   # Status
IMAGE_CMD_DLG_COL_SIZE     # Size
IMAGE_CMD_DLG_COL_TYPE     # Type
IMAGE_CMD_DLG_COL_DATE     # Date
IMAGE_CMD_DLG_COL_PATH     # Saved Path
IMAGE_CMD_DLG_COL_FOUND    # Image found at
IMAGE_CMD_DLG_FRAME_PATH   # Image found at
IMAGE_CMD_DLG_IS_LOADED    # Loaded
IMAGE_CMD_DLG_IS_UNLOAD    # Unloaded
IMAGE_CMD_DLG_IS_RELOAD    # Reloaded
IMAGE_CMD_DLG_IS_NOTFOUND  # Not found
IMAGE_CMD_DLG_ERR_NOFILE1  # Image file not found !
IMAGE_CMD_DLG_ERR_NOFILE2  # Do you really want to use this path ?

//------ IMAGE CMDLINE
IMAGE_CMD_CMDLINE          # -IMAGE
CMD_-IMAGE_MSG_OPTION      # Enter image option [?/Detach/Path/Reload/Unload/Attach] <Attach>:
CMD_-IMAGE_KWD_OPTION      # ? Detach Path Reload Unload Attach
CMD_-IMAGE_?_LISTMASK      # Images to list <*>:
CMD_-IMAGE_?_NO_IMAGES     # No images in current drawing.
CMD_-IMAGE_?_NOSUCHIMAGES  # No matching image names found.
CMD_-IMAGE_?_HEADER1       # Image Name                       Instances  Path
CMD_-IMAGE_?_HEADER2       # ----------------                 ---------  ----------
CMD_-IMAGE_?_NR_IMAGEDEFS  # Total image definitions:
CMD_-IMAGE_?_NR_IMAGES     # Total image instances:
CMD_-IMAGE_P_LISTMASK      # Enter list of images for path modification:
CMD_-IMAGE_P_MSGNAME       # Image name:
CMD_-IMAGE_P_OLDPATH       # Old path:
CMD_-IMAGE_P_NEWPATH       # Enter New path:
CMD_-IMAGE_P_CHANGED       # Path changed.
CMD_-IMAGE_P_NOTCHANGED    # Path unchanged.
CMD_-IMAGE_P_FILENOTFOUND  # Note: this file can not be found !
CMD_-IMAGE_D_LISTMASK      # Enter list of images to detach:
CMD_-IMAGE_R_LISTMASK      # Enter list of images to reload:
CMD_-IMAGE_R_RELOADMSG     # Reload image
CMD_-IMAGE_R_RELOADING     #   Reloading...
CMD_-IMAGE_R_RELOADED      # loaded and relinked.
CMD_-IMAGE_U_LISTMASK      # Enter list of images to unload:
CMD_-IMAGE_A_FILENAME      # Enter image file name to attach:
CMD_-IMAGE_A_IMGNOTFOUND   # Image not found.

//------ IMAGECLIP
IMAGECLIP_CMD              # IMAGECLIP
SELECT_IMAGE_TO_CLIP       # Select image to clip:
OPTION_IMAGE_TO_CLIP       # Enter image clipping option [ON/OFF/Delete/New boundary] <New>:
OPTION_IMAGE_TO_CLIP_KEYS  # ON OFF Delete New
OPTION_IMAGE_TO_CLIP_OLD   # Delete old boundary? [No/Yes] <Yes>:
KEYWRD_IMAGE_TO_CLIP_OLDK  # Yes No
OPTION_IMAGE_TO_CLIP_MODE  # Enter clipping type [Polygonal/Rectangular] <Rectangular>:
KEYWRD_IMAGE_TO_CLIP_MODEK # Rectangular Polygonal
RECTANGLE_CLIP_POINT_1     # Specify first corner point:
RECTANGLE_CLIP_POINT_2     # Specify opposite corner point:
RECTANGLE_CLIP_POINT_FIRST # Specify first point:
RECTANGLE_CLIP_POINT_NEXT  # Specify next point or [Close]:
RECTANGLE_CLIP_KEY_CLOSE   # Close

//------ IMAGETRANSPARENCY
IMAGETRANSPARENCY_CMD      # TRANSPARENCY
SELECT_IMAGE_TO_TRANSP     # Select image(s):
SELECT_TRANSPARENCY_MODE   # Enter transparency mode [ON/OFF]
KEYWRD_TRANSPARENCY_ON     # ON
KEYWRD_TRANSPARENCY_OFF    # OFF

//------ IMAGEQUALITY
IMAGEQUALITY_CMD           # IMAGEQUALITY
SELECT_IMGQUALITY_MODE     # Enter image quality setting [High/Draft]
SELECT_IMGQUALITY_KEYHIGH  # High
SELECT_IMGQUALITY_KEYDRAFT # Draft

//------ IMAGEADJUST
IMAGEADJUST_CMD            # IMAGEADJUST
IMAGEADJUST_CMD_CMDLINE    # -IMAGEADJUST
SELECT_IMAGE_TO_ADJUST     # Select image:
DLG_IMGADJ_TITLE           # Image Adjust
DLG_IMGADJ_BRIGHT_LABEL    # Brightness
DLG_IMGADJ_BRIGHT_DARK     # Dark
DLG_IMGADJ_BRIGHT_LIGHT    # Light
DLG_IMGADJ_CONTRAST_LABEL  # Contrast
DLG_IMGADJ_CONTRAST_LOW    # Low
DLG_IMGADJ_CONTRAST_HIGH   # High
DLG_IMGADJ_FADE_LABEL      # Fade
DLG_IMGADJ_FADE_MIN        # Min
DLG_IMGADJ_FADE_MAX        # Max
DLG_IMGADJ_RESET           # ZReset
DLG_IMGADJ_UNDO            # Undo
DLG_IMGADJ_OK              # OK
DLG_IMGADJ_CANCEL          # Cancel
CMD_IMGADJ_SELECT_OPTION   # Enter image option [Contrast/Fade/Brightness] <Brightness>:
CMD_IMGADJ_KEYWRDS         # Contrast Fade Brightness
CMD_IMGADJ_OPTION_BRIGHT   # Enter brightness value (0-100)
CMD_IMGADJ_OPTION_CONTRAST # Enter contrast value (0-100)
CMD_IMGADJ_OPTION_FADE     # Enter fade value (0-100)
CMD_IMGADJ_ERROR_VALUE     # Value must be an integer between 0 and 100.

//------ IMAGEATTACH
IMAGEATTACH_CMD            # IMAGEATTACH
DLG_IMGINS_TITLE           # Image
DLG_IMGINS_MSG_NAME        # Name
DLG_IMGINS_BTN_FILE        # Browse...
DLG_IMGINS_CHK_PATH        # Retain Path
DLG_IMGINS_MSG_PATH        # Path
DLG_IMGINS_FRM_POINT       # Insertion Point
DLG_IMGINS_CHK_POINT       # Specify on-screen
DLG_IMGINS_MSG_X           # X:
DLG_IMGINS_MSG_Y           # Y:
DLG_IMGINS_MSG_Z           # Z:
DLG_IMGINS_FRM_SCALE       # Scale
DLG_IMGINS_CHK_SCALE       # Specify on-screen
DLG_IMGINS_MSG_SCALE       # Factor
DLG_IMGINS_FRM_ROT         # Rotation
DLG_IMGINS_CHK_ROT         # Specify on-screen
DLG_IMGINS_MSG_ROT         # Angle
DLG_IMGINS_BTN_OK          # OK
DLG_IMGINS_BTN_CANCEL      # Cancel
DLG_IMGINS_SELECTTITLE     # Select Image file
DLG_IMGINS_SELECTFILTER    # All image files|*.bmp;*.rle;*.dib;*.rst;*.gp4;*.mil;*.cal;*.cg4;*.flc;*.fli;*.bil;*.ig4;*.igs;*.jpg;*.pcx;*.pct;*.png;*.rlc;*.tga;*.tif;*.tiff|All Files (*.*)|*.*|BMP (*.bmp, *.rle, *.dib)|*.bmp;*.rle;*.dib|CALS1 (*.rst, *.gp4, *.mil, *.cal, *.cg4)|*.rst;*.gp4;*.mil;*.cal;*.cg4|FLIC (*.flc, *.fli)|*.flc;*.fli|GEOSPOT (*.bil)|*.bil|IG4 (*.ig4)|*.ig4|IGS (*.igs)|*.igs|JPEG (*.jpg)|*.jpg|PCX (*.pcx)|*.pcx|PICT (*.pct)|*.pct|PNG (*.png)|*.png|RLC (*.rlc)|*.rlc|TGA (*.tga)|*.tga|TIFF (*.tif, *.tiff)|*.tif;*.tiff||
DLG_IMGINS_INSERTPOINT     # Specify insertion point :
DLG_IMGINS_INSERTSCALE     # Specify scale factor :
DLG_IMGINS_INSERTROTATION  # Specify rotation angle :

//----- 3DMESH
3DMESH_CMD            # 3DMESH
3DMESH_M_POINTS       # Enter size of mesh in M (rows) direction
3DMESH_N_POINTS       # Enter size of mesh in N (cols) direction
3DMESH_X_POINT_IN     # Specify location for vertex

//----- PFACE
PFACE_CMD             # PFACE
PFACE_MSG_LOCATION1   # Specify location for vertex %d
PFACE_MSG_LOCATION2   # Specify location for vertex %d or <define faces>
PFACE_MSG_FACEVERTEX  # Face %d, vertex %d
PFACE_MSG_VTXCOLLAY     # Enter a vertex number or [Color/Layer]
PFACE_MSG_VTXCOLLAYNEXT # Enter a vertex number or [Color/Layer] <next face>
PFACE_KWD_COLLAY      # Color Layer
PFACE_KWD_COLLAY_LOC  # Color Layer
PFACE_ERR_INVAL_VTX   # Invalid vertex.
PFACE_ERR_INVAL_INT   # Requires an integer between -%d and %d, or option keyword.

//----- TRACE
TRACE_CMD             # TRACE
TRACE_MSG_WIDTH       # Specify trace width
TRACE_MSG_START       # Specify start point
TRACE_MSG_NEXT        # Specify next point

//----- 3DFACE
3DFACE_CMD            # 3DFACE
3DFACE_MSG_PT1        # Specify first point
3DFACE_MSG_PT2        # Specify second point
3DFACE_MSG_PT3        # Specify third point
3DFACE_MSG_PT4        # Specify fourth point
3DFACE_MSG_INVIS      # or [Invisible]
3DFACE_KWD_INVIS      # Invisible
3DFACE_KWD_INVIS_LOC  # Invisible
3DFACE_MSG_EXIT       # <exit>
3DFACE_MSG_3SIDE      # <create three-sided face>

//----- EDGE
EDGE_CMD              # EDGE
EDGE_KWD_DISPLAY      # Display
EDGE_KWD_DISPLAY_LOC  # Display
EDGE_MSG_START        # Specify edge of 3dface to toggle visibility or [Display]
EDGE_MSG_SELECT       # Enter selection method for display of hidden edges [Select/All] <All>
EDGE_KWD_ALL          # All
EDGE_KWD_ALL_LOC      # All
EDGE_KWD_SELECT       # Select
EDGE_KWD_SELECT_LOC   # Select
EDGE_MSG_REGEN        # * Regenerating 3DFACE objects ...
EDGE_MSG_DONE         # done.
EDGE_MSG_NOEDGE       # No 3DFACE edges found.
EDGE_MSG_NOFACES      # No 3DFACE objects found.

//----- TABSURF
TABSURF_CMD           # TABSURF
TABSURF_SURFTAB1      # Current wire frame density
TABSURF_MSG_PATH      # Select object for path curve
TABSURF_MSG_VECTOR    # Select object for direction vector
TABSURF_ERR_PATH      # Path curve object not usable to define tabulated surface.
TABSURF_ERR_VECTOR    # Object not usable as direction vector.

//----- REVSURF
REVSURF_CMD           # REVSURF
REVSURF_SURFTAB12     # Current wire frame density
REVSURF_MSG_PATH      # Select object to revolve
REVSURF_MSG_VECTOR    # Select object that defines the axis of revolution
REVSURF_MSG_STARTANG  # Specify start angle
REVSURF_MSG_INCLANG   # Specify included angle (+=ccw, -=cw)
REVSURF_ERR_PATH      # Object not usable to define surface of revolution.
REVSURF_ERR_VECTOR    # Object not usable as rotation axis.

//----- RULESURF
RULESURF_CMD          # RULESURF
RULESURF_SURFTAB1     # Current wire frame density
RULESURF_MSG_PATH1    # Select first defining curve
RULESURF_MSG_PATH2    # Select second defining curve
RULESURF_ERR_PATH     # Object not usable to define ruled surface.
RULESURF_ERR_2POINTS  # Two points don't define a surface.
RULESURF_ERR_MIXED    # Cannot mix closed and open paths.

//----- EDGESURF
EDGESURF_CMD          # EDGESURF
EDGESURF_SURFTAB12    # Current wire frame density
EDGESURF_MSG_PATH     # Select object %d for surface edge
EDGESURF_ERR_PATH     # Object not usable to define surface patch.
EDGESURF_ERR_ISPICK   # You already picked that object.
EDGESURF_ERR_GAP      # Edge %d does not touch another edge.

//----- VPCLIP
VPCLIP_CMD                   # VPCLIP
VPCLIP_MSG_SEL_VPORT         # Select viewport to clip
VPCLIP_ERR_SEL_VPORT         # Object selected was not a viewport.
VPCLIP_ERR_SEL_CLIPENT       # Invalid viewport clip object.
VPCLIP_ERR_SEL_NOTCLOSED     # Clip object must be closed.
VPCLIP_KWD_SEL_CLIP_POLY     # Polygonal
VPCLIP_KWD_SEL_CLIP_POLY_LOC # Polygonal
VPCLIP_KWD_SEL_CLIP_DEL      # Delete
VPCLIP_KWD_SEL_CLIP_DEL_LOC  # Delete
VPCLIP_MSG_SEL_CLIP0         # Select clipping object or [Polygonal] <Polygonal>
VPCLIP_MSG_SEL_CLIP1         # Select clipping object or [Polygonal/Delete] <Polygonal>

//----- ARX
ARX_CMD             # ARX
ARX_MSG_START       # Enter an option [?/Load/Unload/Commands]
ARX_KWD_START       # ? Load Unload Commands
ARX_MSG_COMMANDS    # Commands Registered by Extension Programs
ARX_MSG_CMDGROUP    # Command Group
ARX_MSG_CMDNEXT     # Press ENTER to continue
ARX_MSG_LOADED      # Loaded Runtime Extension Programs
ARX_MSG_LOADEDEND   # End of List.
ARX_MSG_UNLOAD      # Enter ARX/DBX/DLL file name to unload
ARX_MSG_UNLOAD_YES  # %s successfully unloaded.
ARX_MSG_UNLOAD_NO   # Application %s is locked and could not be unloaded.
ARX_MSG_DLGTITLE    # Select ARX/DBX/DLL File
ARX_MSG_LOADFILE    # Enter ARX/DBX/DLL file name to load
ARX_ERR_NOFILEFOUND # Cannot find the specified file
ARX_MSG_LOADED_YES  # %s successfully loaded.
ARX_MSG_LOADED_NO   # %s not loaded - incompatible version or unresolved dependencies.
ARX_MSG_ARXLOCKED   # Application '%s' is locked and could not be unloaded.
ARX_MSG_NOTLOADED   # Unable to unload application '%s' because it's not loaded.

//----- MINSERT
MINSERT_CMD             # MINSERT
MINSERT_MSG_START       # Enter block name or [?]
MINSERT_KWD_START       # ?
MINSERT_ERR_BLOCKNAME   # block is not defined. Can not find dwg file for block.
MINSERT_ERR_INVALID     # Invalid block name.
MINSERT_ERR_NOEXPLODE   # ** Error: Cannot explode blocks with the MINSERT command.
MINSERT_MSG_LISTBLOCKS  # Enter block(s) to list <*>
MINSERT_MSG_DEFBLOCKS1  # Defined blocks
MINSERT_MSG_DEFBLOCKS2  # User     Unnamed
MINSERT_MSG_DEFBLOCKS3  # Blocks   Blocks
MINSERT_MSG_NUMBERROWS  # Enter number of rows (---) <1>
MINSERT_MSG_NUMBERCOLS  # Enter number of columns (|||) <1>
MINSERT_MSG_ROWCELLDIST # Enter distance between rows or specify unit cell (---)
MINSERT_MSG_COLCELLDIST # Specify distance between columns (|||)
MINSERT_MSG_SETATTRIBS  # Enter attribute values

//----- XCLIP
XCLIP_CMD             # XCLIP
XCLIP_MSG_START0      # Enter clipping option
XCLIP_MSG_START1      # [ON/OFF/Clipdepth/Delete/generate Polyline/New boundary] <New>
XCLIP_KWD_START       # ON OFF Clipdepth Delete Polyline New
XCLIP_MSG_DELETEOLD   # Delete old boundary(s)? [Yes/No] <Yes>:
XCLIP_MSG_CLIP0       # Specify clipping boundary
XCLIP_MSG_CLIP1       # [Select polyline/Polygonal/Rectangular] <Rectangular>
XCLIP_MSG_CLIP2       # Specify first corner
XCLIP_MSG_CLIP3       # Specify opposite corner
XCLIP_MSG_CLIP4       # Select polyline
XCLIP_MSG_CLIP5       # Specify first point
XCLIP_MSG_CLIP6       # Specify next point or [Undo]
XCLIP_KWD_CLIPNEW     # Select Polygonal Rectangular
XCLIP_KWD_YESNO       # Yes No
XCLIP_KWD_UNDO        # Undo
XCLIP_MSG_CLIPFRONT   # Specify front clip point or [Distance/Remove]:
XCLIP_MSG_CLIPBACK    # Specify back clip point or [Distance/Remove]:
XCLIP_MSG_CLIPDIST    # Specify distance from boundary
XCLIP_KWD_CLIPDEPTH   # Distance Remove
XCLIP_ERR_NOBLOCKXREF # None found.
XCLIP_ERR_INELIGIBLE  # Object(s) not ineligible for clipping.
XCLIP_ERR_NOBOUNDARY  # No clip boundary found.
XCLIP_ERR_NEEDBOUND   # Xref must contain a clip boundary before specifying a clipdepth.
XCLIP_ERR_CLIPPLANE   # Error: Front clipping plane behind back clipping plane.  Clipdepth not applied.
XCLIP_ERR_SELFISECT1  # Invalid point, polygon segments cannot intersect.
XCLIP_ERR_SELFISECT2  # Self-intersecting clip boundaries are not acceptable.
XCLIP_ERR_NOPOLYLINE  # Selected object is not a polyline.

//----- 3DARRAY
3DARRAY_CMD           # 3DARRAY
3DARRAY_MSG_MODE      # Enter the type of array [Rectangular/Polar] <R>
3DARRAY_KEY_MODE      # Rectangular Polar
3DARRAY_MSG_NRROWS    # Enter the number of rows (---) <1>
3DARRAY_MSG_NRCOLUMNS # Enter the number of columns (|||) <1>
3DARRAY_MSG_NRLEVELS  # Enter the number of levels (...) <1>
3DARRAY_MSG_DISTROW   # Specify the distance between rows (---)
3DARRAY_MSG_DISTCOL   # Specify the distance between columns (|||)
3DARRAY_MSG_DISTLEV   # Specify the distance between levels (...)
3DARRAY_MSG_NRITEMS   # Enter the number of items in the array
3DARRAY_MSG_ANGLEFILL # Specify the angle to fill (+=ccw, -=cw) <360>
3DARRAY_MSG_DOROTATE  # Rotate arrayed objects? [Yes/No] <Y>
3DARRAY_KEY_YESNO     # Yes No
3DARRAY_MSG_AXISP1    # Specify center point of array
3DARRAY_MSG_AXISP2    # Specify second point on axis of rotation
3DARRAY_ERR_NOTHING   # One-element array, nothing to do.
3DARRAY_ERR_MOREITEMS # Number of items must be greater than 1

//----- ALIGN
ALIGN_CMD             # ALIGN
ALIGN_MSG_SOURCE1     # Specify first source point
ALIGN_MSG_TARGET1     # Specify first destination point
ALIGN_MSG_SOURCE2     # Specify second source point
ALIGN_MSG_TARGET2     # Specify second destination point
ALIGN_MSG_SOURCE3     # Specify third source point or <continue>
ALIGN_MSG_TARGET3     # Specify third destination point
ALIGN_MSG_DOSCALING   # Scale objects based on alignment points? [Yes/No] <N>
ALIGN_KEY_DOSCALING   # Yes No
ALIGN_ERR_COINC_S12   # The first and second source points coincide.
ALIGN_ERR_COINC_S23   # The second and third source points coincide.
ALIGN_ERR_COINC_S13   # The first and third source points coincide.
ALIGN_ERR_COINC_T12   # The first and second target points coincide.
ALIGN_ERR_COINC_T23   # The second and third target points coincide.
ALIGN_ERR_COINC_T13   # The first and third target points coincide.

//----- ROTATE3D
ROTATE3D_CMD             # ROTATE3D
ROTATE3D_MSG_CURMODE     # Current positive angle
ROTATE3D_MSG_CLOCKWISE   # clockwise
ROTATE3D_MSG_CCLOCKWISE  # counterclockwise
ROTATE3D_MSG_MAINOPTION1 # Specify first point on axis or define axis by
ROTATE3D_MSG_MAINOPTION2 # [Object/Last/View/Xaxis/Yaxis/Zaxis/2points] <2points>
ROTATE3D_KEY_MAINOPTION  # Object Last View Xaxis Yaxis Zaxis 2points
ROTATE3D_MSG_POINT1      # Specify first point on axis
ROTATE3D_MSG_POINT2      # Specify second point on axis
ROTATE3D_MSG_ROTATION    # Specify rotation angle or [Reference]
ROTATE3D_KEY_ROTATION    # Reference
ROTATE3D_MSG_VIEW        # Specify a point on the view direction axis <0,0,0>
ROTATE3D_MSG_XAXIS       # Specify a point on the X axis <0,0,0>
ROTATE3D_MSG_YAXIS       # Specify a point on the Y axis <0,0,0>
ROTATE3D_MSG_ZAXIS       # Specify a point on the Z axis <0,0,0>
ROTATE3D_MSG_ANGREF      # Specify the reference angle
ROTATE3D_MSG_ANGNEW      # Specify the new angle
ROTATE3D_MSG_SELOBJECT   # Select a line, circle, arc, or 2D-polyline segment
ROTATE3D_ERR_NOLASTAXIS  # No axis has been used before.
ROTATE3D_ERR_NOOBJECT    # No object picked.
ROTATE3D_ERR_WRONGOBJECT # Improper type of object picked.

//----- MIRROR3D
MIRROR3D_CMD             # MIRROR3D
MIRROR3D_MSG_MAINOPTION1 # Specify first point of mirror plane (3 points) or
MIRROR3D_MSG_MAINOPTION2 # [Object/Last/Zaxis/View/XY/YZ/ZX/3points] <3points>
MIRROR3D_KEY_MAINOPTION  # Object Last Zaxis View XY YZ ZX 3points
MIRROR3D_MSG_POINT1      # Specify first point on mirror plane
MIRROR3D_MSG_POINT2      # Specify second point on mirror plane
MIRROR3D_MSG_POINT3      # Specify third point on mirror plane
MIRROR3D_MSG_POINTPLANE  # Specify point on mirror plane
MIRROR3D_MSG_POINTZAXIS  # Specify point on Z-axis (normal) of mirror plane
MIRROR3D_MSG_VIEW        # Specify point on view plane <0,0,0>
MIRROR3D_MSG_XYPLANE `   # Specify point on XY plane <0,0,0>
MIRROR3D_MSG_YZPLANE     # Specify point on YZ plane <0,0,0>
MIRROR3D_MSG_ZXPLANE     # Specify point on ZX plane <0,0,0>
MIRROR3D_MSG_SELOBJECT   # Select a circle, arc, or 2D-polyline segment
MIRROR3D_MSG_DELOBJECT   # Delete source objects? [Yes/No] <N>
MIRROR3D_KEY_DELOBJECT   # Yes No
MIRROR3D_ERR_NOLASTPLANE # No plane has been used before.
MIRROR3D_ERR_NOOBJECT    # No object picked.
MIRROR3D_ERR_WRONGOBJECT # Improper type of object picked.

//----- SKETCH
SKETCH_CMD            # SKETCH
SKETCH_MSG_MODE       # Current sketch entities
SKETCH_MSG_INCREMENT  # Record increment
SKETCH_MSG_MAIN       # Sketch:  Pen eXit Quit Record Erase Connect .
SKETCH_KEY_PEN        # Pen
SKETCH_KEY_EXIT       # eXit
SKETCH_KEY_QUIT       # Quit
SKETCH_KEY_RECORD     # Record
SKETCH_KEY_ERASE      # Erase
SKETCH_KEY_CONNECT    # Connect
SKETCH_KEY_CONTINUE   # .
SKETCH_MSG_PENDOWN    # <Pen down>
SKETCH_MSG_PENUP      # <Pen up>
SKETCH_MSG_DONELINES  # %d line(s) recorded.
SKETCH_MSG_DONEPOLYS  # %d polyline(s) with %d edges recorded.
SKETCH_MSG_MOVETOLINE # Connect: Move to endpoint of line.
SKETCH_MSG_SELECTEND  # Erase:  Select end of delete.
SKETCH_ERR_NOLASTPNT  # No last point known.
SKETCH_ERR_CONNECT    # Connect aborted.
SKETCH_ERR_ERASE      # Erase aborted.
SKETCH_ERR_RECORDED   # Nothing recorded.
SKETCH_ERR_SKETCHED   # Nothing sketched.

//----- ATTREDEF
ATTREDEF_CMD            # ATTREDEF
ATTREDEF_CMD_ALT        # AT
ATTREDEF_MSG_BLOCKNAME  # Enter name of the block you wish to redefine
ATTREDEF_ERR_BLOCKNAME  # block is not defined. Please try again.
ATTREDEF_ERR_NOINSERTS  # No insertions of block %s found to redefine.
ATTREDEF_MSG_NEWBLOCK   # Select objects for new Block ...
ATTREDEF_ERR_NEWBLOCK   # No new Block selected. Please try again.
ATTREDEF_ERR_NOATTRIBS  # New block has no attributes.
ATTREDEF_MSG_REFPOINT   # Specify insertion base point of new Block

//----- Acis common errors
ACIS_ERR_UNKNOW_ERR   # * ACIS error : unknown error
ACIS_ERR_INVALIDINP   # * ACIS error : invalid input
ACIS_ERR_OUTOFRANGE   # * ACIS error : data out of range
ACIS_ERR_INVALIDAXIS  # * ACIS error : invalid axis
ACIS_ERR_MISSINGACIS0 # * ACIS System incomplete
ACIS_ERR_MISSINGACIS1 # * ACIS system is not completely installed / available !
ACIS_ERR_MISSINGACIS2 # SUGGESTION: please install the AutoCAD 2004 Trial version (30 days trial)
ACIS_ERR_MISSINGACIS3 # on this PC. It is NOT nessecary to copy any files or to set/add any
ACIS_ERR_MISSINGACIS4 # support pathes - LTE Kernel will automatically activate ACIS system.

//----- BOX (Acis)
ACIS_BOX_CMD          # BOX
ACIS_BOX_MSG_POINT1   # Specify corner of box or [CEnter] <0,0,0>
ACIS_BOX_KEY_POINT1   # CEnter
ACIS_BOX_MSG_CENTER   # Specify center of box <0,0,0>
ACIS_BOX_MSG_CORNER   # Specify corner or [Cube/Length]
ACIS_BOX_KEY_CORNER   # Cube Length
ACIS_BOX_MSG_LENGTH   # Specify length
ACIS_BOX_MSG_WIDTH    # Specify width
ACIS_BOX_MSG_HEIGHT   # Specify height

//----- WEDGE (Acis)
ACIS_WEDGE_CMD        # WEDGE
ACIS_WEDGE_MSG_POINT1 # Specify corner of wedge or [CEnter] <0,0,0>
ACIS_WEDGE_KEY_POINT1 # CEnter
ACIS_WEDGE_MSG_CENTER # Specify center of wedge <0,0,0>
ACIS_WEDGE_MSG_CORNER # Specify opposite corner or [Cube/Length]
ACIS_WEDGE_KEY_CORNER # Cube Length
ACIS_WEDGE_MSG_LENGTH # Specify length
ACIS_WEDGE_MSG_WIDTH  # Specify width
ACIS_WEDGE_MSG_HEIGHT # Specify height

//----- SPHERE (Acis)
ACIS_SPHERE_CMD        # SPHERE
ACIS_SPHERE_MSG_CENTER # Specify center of sphere <0,0,0>
ACIS_SPHERE_MSG_RADIUS # Specify radius of sphere or [Diameter]
ACIS_SPHERE_MSG_DIAM   # Specify diameter
ACIS_SPHERE_KEY_DIAM   # Diameter

//----- CONE (Acis)
ACIS_CONE_CMD         # CONE
ACIS_CONE_MSG_CENTER  # Specify center point for base of cone or [Elliptical] <0,0,0>
ACIS_CONE_KEY_ELLIPSE # Elliptical
ACIS_CONE_MSG_POINT1  # Specify axis endpoint of ellipse for base of cone or [Center]
ACIS_CONE_MSG_POINT2  # Specify second axis endpoint of ellipse for base of cone
ACIS_CONE_KEY_CENTER  # Center
ACIS_CONE_MSG_POINTX1 # Specify center point of ellipse for base of cone <0,0,0>
ACIS_CONE_MSG_POINTX2 # Specify axis endpoint of ellipse for base of cone
ACIS_CONE_MSG_LENGTH  # Specify length of other axis for base of cone
ACIS_CONE_MSG_RADIUS  # Specify radius for base of cone or [Diameter]
ACIS_CONE_KEY_DIAM    # Diameter
ACIS_CONE_MSG_DIAM    # Specify diameter for base of cone
ACIS_CONE_MSG_HEIGHT  # Specify height of cone or [Apex]
ACIS_CONE_KEY_APEX    # Apex
ACIS_CONE_MSG_APEX    # Specify apex point

//----- CYLINDER (Acis)
ACIS_CYLINDER_CMD    # CYLINDER
ACIS_CYL_MSG_CENTER  # Specify center point for base of cylinder or [Elliptical] <0,0,0>
ACIS_CYL_KEY_ELLIPSE # Elliptical
ACIS_CYL_MSG_POINT1  # Specify axis endpoint of ellipse for base of cylinder or [Center]
ACIS_CYL_MSG_POINT2  # Specify second axis endpoint of ellipse for base of cylinder
ACIS_CYL_KEY_CENTER  # Center
ACIS_CYL_MSG_POINTX1 # Specify center point of ellipse for base of cylinder <0,0,0>
ACIS_CYL_MSG_POINTX2 # Specify axis endpoint of ellipse for base of cylinder
ACIS_CYL_MSG_LENGTH  # Specify length of other axis for base of cylinder
ACIS_CYL_MSG_RADIUS  # Specify radius for base of cylinder or [Diameter]
ACIS_CYL_KEY_DIAM    # Diameter
ACIS_CYL_MSG_DIAM    # Specify diameter for base of cylinder
ACIS_CYL_MSG_HEIGHT  # Specify height of cylinder or [Center of other end]
ACIS_CYL_KEY_APEX    # Center
ACIS_CYL_MSG_APEX    # Specify center of other end of cylinder

//----- TORUS (Acis)
ACIS_TORUS_CMD        # TORUS
ACIS_TORUS_MSG_CENTER # Specify center of torus <0,0,0>
ACIS_TORUS_MSG_RADMAX # Specify radius of torus or [Diameter]
ACIS_TORUS_MSG_RADMIN # Specify radius of tube or [Diameter]
ACIS_TORUS_MSG_DIAM   # Specify diameter
ACIS_TORUS_KEY_DIAM   # Diameter

//----- EXTRUDE (Acis)
ACIS_EXTRUDE_CMD        # EXTRUDE
ACIS_EXTRUDE_MSG_HEIGHT # Specify height of extrusion or [Path]
ACIS_EXTRUDE_MSG_TAPER  # Specify angle of taper for extrusion <0>
ACIS_EXTRUDE_MSG_PATH   # Select extrusion path or [Taper angle]
ACIS_EXTRUDE_MSG_PATH1  # Select extrusion path
ACIS_EXTRUDE_KEY_PATH   # Path
ACIS_EXTRUDE_KEY_TAPER  # Taper
ACIS_EXTRUDE_ERR_NOENT  # Nothing selected.
ACIS_EXTRUDE_ERR_FAIL   # Unable to extrude the selected object.
ACIS_EXTRUDE_ERR_FAIL1  # Unable to extrude %d selected object(s).
ACIS_EXTRUDE_ERR_PATH   # Cannot extrude along this path.
ACIS_EXTRUDE_ERR_ANGLE  # Angle must be greater than -90 degrees and less than 90 degrees.

//----- REVOLVE (Acis)
ACIS_REVOLVE_CMD        # REVOLVE
ACIS_REVOLVE_MSG_AXIS1  # Specify start point for axis of revolution or
ACIS_REVOLVE_MSG_AXIS2  # define axis by [Object/X (axis)/Y (axis)/Z (axis)]
ACIS_REVOLVE_MSG_AXIS3  # Specify endpoint of axis
ACIS_REVOLVE_KEY_AXIS   # Object X Y Z
ACIS_REVOLVE_MSG_ANGLE  # Specify angle of revolution <360>
ACIS_REVOLVE_MSG_ENTITY # Select an object
ACIS_REVOLVE_ERR_NOENT  # Nothing selected.
ACIS_REVOLVE_ERR_FAIL   # Unable to revolve the selected object.
ACIS_REVOLVE_ERR_FAIL1  # Unable to revolve %d selected object(s).
ACIS_REVOLVE_ERR_AXIS1  # Axis must be a line or a polyline.
ACIS_REVOLVE_ERR_AXIS2  # That polyline does not define an axis.

//----- INTERFERE (Acis)
ACIS_INTERFERE_CMD         # INTERFERE
ACIS_INTERFERE_MSG_SET1    # Select first set of solids
ACIS_INTERFERE_MSG_SET2    # Select second set of solids
ACIS_INTERFERE_MSG_COMP1   # * Comparing %d solids with each other.
ACIS_INTERFERE_MSG_COMP2   # * Comparing %d solid(s) against %d solid(s).
ACIS_INTERFERE_MSG_NOTINT  # Solids do not interfere.
ACIS_INTERFERE_MSG_INTER1  # * Interfering solids (first  set) :
ACIS_INTERFERE_MSG_INTER2  # *                    (second set) :
ACIS_INTERFERE_MSG_INTER3  # * Interfering pairs               :
ACIS_INTERFERE_MSG_INTER4  # * Interfering solids :
ACIS_INTERFERE_MSG_INTER5  # * Interfering pairs  :
ACIS_INTERFERE_MSG_CREATE  # Create interference solids? [Yes/No] <N>
ACIS_INTERFERE_MSG_SHOW    # Highlight pairs of interfering solids? [Yes/No] <N>
ACIS_INTERFERE_KEY_YESNO   # Yes No
ACIS_INTERFERE_MSG_SHOWOPT # Enter an option [Next pair/eXit] <Next>
ACIS_INTERFERE_KEY_SHOWOPT # Next eXit
ACIS_INTERFERE_ERR_NOENT   # No solids selected.
ACIS_INTERFERE_ERR_SINGLE  # Only one solid selected in the first set.

//----- SOLIDEDIT (Acis)
ACIS_SOLIDEDIT_CMD              # SOLIDEDIT
ACIS_SOLEDIT_MSG_CHECKING       # Solids editing automatic checking
ACIS_SOLEDIT_MSG_MAINOPTIONS    # Enter a solids editing option [Face/Edge/Body/Undo/eXit] <eXit>
ACIS_SOLEDIT_KEY_MAINOPTIONS    # Face Edge Body Undo eXit
ACIS_SOLEDIT_MSG_FACEOPTIONS    # Enter a face editing option [Extrude/Move/Rotate/Offset/Taper/Delete/Copy/coLor/Undo/eXit] <eXit>
ACIS_SOLEDIT_KEY_FACEOPTIONS    # Extrude Move Rotate Offset Taper Delete Copy coLor Undo eXit
ACIS_SOLEDIT_MSG_EDGEOPTIONS    # Enter an edge editing option [Copy/coLor/Undo/eXit] <eXit>
ACIS_SOLEDIT_KEY_EDGEOPTIONS    # Copy coLor Undo eXit
ACIS_SOLEDIT_MSG_BODYOPTIONS    # Enter a body editing option [Imprint/seParate solids/Shell/cLean/Check/Undo/eXit] <eXit>
ACIS_SOLEDIT_KEY_BODYOPTIONS    # Imprint seParate Shell cLean Check Undo eXit
ACIS_SOLEDIT_MSG_UNDOCOMPLETE   # Command has been completely undone.
ACIS_SOLEDIT_MSG_SELECTSOLID    # Select a 3D solid
ACIS_SOLEDIT_ERR_SELECTSOLID    # A 3D solid must be selected.
ACIS_SOLEDIT_ERR_NOSOLIDDETECT  # No solids detected.
ACIS_SOLEDIT_MSG_SELECTEDGES    # Select edges or [Undo/Remove]
ACIS_SOLEDIT_KEY_SELECTEDGES    # Undo Remove
ACIS_SOLEDIT_MSG_SELECTFACES    # Select faces or [Undo/Remove]
ACIS_SOLEDIT_KEY_SELECTFACES    # Undo Remove
ACIS_SOLEDIT_MSG_SELECTFACESALL # Select faces or [Undo/Remove/ALL]
ACIS_SOLEDIT_KEY_SELECTFACESALL # Undo Remove ALL
ACIS_SOLEDIT_MSG_IMPRINTSOURCE  # Select an object to imprint
ACIS_SOLEDIT_MSG_IMPRINTSOURCEN # Delete the source object [Yes/No] <N>
ACIS_SOLEDIT_MSG_IMPRINTSOURCEY # Delete the source object [Yes/No] <Y>
ACIS_SOLEDIT_KEY_YESNO          # Yes No
ACIS_SOLEDIT_MSG_VALIDSOLID     # This object is a valid ShapeManager solid.
ACIS_SOLEDIT_MSG_INVALIDSOLID   # This object is *NOT* a valid ShapeManager solid.
ACIS_SOLEDIT_ERR_SELECTEDGEORG  # Please select an edge on the originally selected solid.
ACIS_SOLEDIT_ERR_SELECTFACEORG  # Please select an face on the originally selected solid.
ACIS_SOLEDIT_ERR_NOTSEPARATABLE # The selected solid does not have multiple lumps.
ACIS_SOLEDIT_ERR_NOFACESTODO    # There are no faces selected to be processed.
ACIS_SOLEDIT_MSG_REMFACES       # Remove faces or [Undo/Add/ALL]
ACIS_SOLEDIT_MSG_SELFACES       # Select faces or [Undo/Remove/ALL]
ACIS_SOLEDIT_MSG_FACEUNDODONE   # Face selection has been completely undone.
ACIS_SOLEDIT_MSG_NRFACESADDED   # %d faces found, %d added.
ACIS_SOLEDIT_MSG_NRFACESREMOVED # %d faces found, %d removed.
ACIS_SOLEDIT_ERR_NOFACESTODO    # There are no faces selected to be processed.
ACIS_SOLEDIT_MSG_REMEDGES       # Remove edges or [Undo/Add/ALL]
ACIS_SOLEDIT_MSG_SELEDGES       # Select edges or [Undo/Remove/ALL]
ACIS_SOLEDIT_MSG_EDGEUNDODONE   # Edge selection has been completely undone.
ACIS_SOLEDIT_MSG_NREDGESADDED   # %d edges found, %d added.
ACIS_SOLEDIT_MSG_NREDGESREMOVED # %d edges found, %d removed.
ACIS_SOLEDIT_ERR_NOEDGESTODO    # There are no edges selected to be processed.
ACIS_SOLEDIT_KEY_UNDOADDALL     # Undo Add ALL
ACIS_SOLEDIT_KEY_UNDOREMALL     # Undo Remove ALL
ACIS_SOLEDIT_MSG_OFFSETDIST     # Enter the shell offset distance
ACIS_SOLEDIT_MSG_FACESEXTHEIGHT # Specify height of extrusion or [Path]
ACIS_SOLEDIT_MSG_FACESEXTTAPER  # Specify angle of taper for extrusion <0>
ACIS_SOLEDIT_MSG_FACESEXTPATH   # Select extrusion path
ACIS_SOLEDIT_KEY_FACESEXTPATH   # Path
ACIS_SOLEDIT_ERR_ANGLE          # Angle must be greater than -90 degrees and less than 90 degrees.
ACIS_SOLEDIT_ERR_NOPATH         # Nothing selected for the path.
ACIS_SOLEDIT_MSG_FACESMOVE1     # Specify a base point of displacement
ACIS_SOLEDIT_MSG_FACESMOVE2     # Specify a second point of displacement
ACIS_SOLEDIT_MSG_FACESROTATE    # Specify an axis point or [Axis by object/View/Xaxis/Yaxis/Zaxis] <2points>
ACIS_SOLEDIT_MSG_FACESROTATEP1  # Specify the first point on the rotation axis
ACIS_SOLEDIT_MSG_FACESROTATEP2  # Specify the second point on the rotation axis
ACIS_SOLEDIT_MSG_FACESROTATEPT  # Specify the origin of the rotation <0,0,0>
ACIS_SOLEDIT_MSG_FACESROTATEANG # Specify a rotation angle or [Reference]
ACIS_SOLEDIT_KEY_FACESROTATE    # 2points Axis View Xaxis Yaxis Zaxis
ACIS_SOLEDIT_KEY_FACESROTATEANG # Reference
ACIS_SOLEDIT_KEY_FACESROTANG1   # Specify the reference (starting) angle <0>
ACIS_SOLEDIT_KEY_FACESROTANG2   # Specify second point
ACIS_SOLEDIT_KEY_FACESROTANG3   # Specify the ending angle
ACIS_SOLEDIT_MSG_FACESROTATEOBJ # Select a curve to be used for the axis
ACIS_SOLEDIT_ERR_FACESROTATEOBJ # Cannot use that entity as a rotation axis.
ACIS_SOLEDIT_MSG_FACESOFFSDIST  # Specify the offset distance
ACIS_SOLEDIT_MSG_FACESTAPERPT1  # Specify the base point
ACIS_SOLEDIT_MSG_FACESTAPERPT2  # Specify another point along the axis of tapering
ACIS_SOLEDIT_MSG_FACESTAPERANG  # Specify the taper angle
ACIS_SOLEDIT_ERR_FACESTAPERANG  # The taper angle must be non-zero and between -90 and 90.

//----- STLOUT (Acis)
ACIS_STLOUT_CMD           # STLOUT
ACIS_STLOUT_MSG_SELECT    # Select a single solid for STL output ..
ACIS_STLOUT_MSG_FILEMODE  # Create a binary STL file? [Yes/No] <Y>
ACIS_STLOUT_KEY_FILEMODE  # Yes No
ACIS_STLOUT_MSG_FILETYPE  # Lithography (*.stl)
ACIS_STLOUT_MSG_FILEDLG   # Create STL file
ACIS_STLOUT_ERR_NOSOLIDS  # No solids selected.
ACIS_STLOUT_ERR_USESINGLE # Only one solid per file permitted.
ACIS_STLOUT_MSG_FILENAME  # Enter STL file name

//----- ACISIN (Acis)
ACIS_ACISIN_CMD           # ACISIN
ACIS_ACISIN_DLG_TITLE     # Select ACIS file
ACIS_ACISIN_FILENAME      # Enter SAT file name

//----- ACISOUT (Acis)
ACIS_ACISOUT_CMD          # ACISOUT
ACIS_ACISOUT_DLG_TITLE    # Create ACIS file
ACIS_ACISOUT_NOTHING      # Nothing selected
ACIS_ACISOUT_NO_SOLIDS    # No solids, regions or ShapeManager bodies selected.
ACIS_ACISOUT_IGNORED      # objects ignored that are not regions, solids or ShapeManager bodies.
ACIS_ACISOUT_FILENAME     # Enter SAT file name

//----- SECTION (Acis)
ACIS_SECTION_CMD          # SECTION
ACIS_SECTION_MSG_MAIN     # Specify first point on Section plane by [Object/Zaxis/View/XY/YZ/ZX/3points] <3points>
ACIS_SECTION_KEY_MAIN     # Object Zaxis View XY YZ ZX 3points
ACIS_SECTION_MSG_POINT1   # Specify first point on plane
ACIS_SECTION_MSG_POINT2   # Specify second point on plane
ACIS_SECTION_MSG_POINT3   # Specify third point on plane
ACIS_SECTION_MSG_POINT_XY # Specify a point on the XY-plane <0,0,0>
ACIS_SECTION_MSG_POINT_YZ # Specify a point on the YZ-plane <0,0,0>
ACIS_SECTION_MSG_POINT_ZX # Specify a point on the ZX-plane <0,0,0>
ACIS_SECTION_MSG_POINT_V  # Specify a point on the current view plane <0,0,0>
ACIS_SECTION_MSG_POINT_Z1 # Specify a point on the section plane
ACIS_SECTION_MSG_POINT_Z2 # Specify a point on the Z-axis (normal) of the plane
ACIS_SECTION_MSG_OBJECT   # Select a circle, ellipse, arc, spline, or polyline
ACIS_SECTION_ERR_NOSOLIDS # No solids selected.
ACIS_SECTION_ERR_NOPLANE  # Unable to extract the plane of the selected object.

//----- SLICE (Acis)
ACIS_SLICE_CMD            # SLICE
ACIS_SLICE_MSG_MAIN       # Specify first point on slicing plane by [Object/Zaxis/View/XY/YZ/ZX/3points] <3points>
ACIS_SLICE_KEY_MAIN       # Object Zaxis View XY YZ ZX 3points
ACIS_SLICE_MSG_POINT1     # Specify first point on plane
ACIS_SLICE_MSG_POINT2     # Specify second point on plane
ACIS_SLICE_MSG_POINT3     # Specify third point on plane
ACIS_SLICE_MSG_POINT_XY   # Specify a point on the XY-plane <0,0,0>
ACIS_SLICE_MSG_POINT_YZ   # Specify a point on the YZ-plane <0,0,0>
ACIS_SLICE_MSG_POINT_ZX   # Specify a point on the ZX-plane <0,0,0>
ACIS_SLICE_MSG_POINT_V    # Specify a point on the current view plane <0,0,0>
ACIS_SLICE_MSG_POINT_Z1   # Specify a point on the section plane
ACIS_SLICE_MSG_POINT_Z2   # Specify a point on the Z-axis (normal) of the plane
ACIS_SLICE_MSG_OBJECT     # Select a circle, ellipse, arc, spline, or polyline
ACIS_SLICE_MSG_SIDE       # Specify a point on desired side of the plane or [keep Both sides]
ACIS_SLICE_KEY_SIDE       # Both
ACIS_SLICE_ERR_NOSOLIDS   # No solids selected.
ACIS_SLICE_ERR_NOPLANE    # Unable to extract the plane of the selected object.

//----- UNION (Acis)
ACIS_UNION_CMD            # UNION
ACIS_UNION_ERR_ENTITIES   # At least 2 solids or coplanar regions must be selected.
ACIS_UNION_ERR_ENTITIESX  # At least 2 coplanar regions must be selected.

//----- INTERSECT (Acis)
ACIS_INTERSECT_CMD           # INTERSECT
ACIS_INTERSECT_ERR_ENTITIES  # At least 2 solids or coplanar regions must be selected.
ACIS_INTERSECT_ERR_ENTITIESX # At least 2 coplanar regions must be selected.
ACIS_INTERSECT_ERR_SOLID     # Null solid created - deleted.
ACIS_INTERSECT_ERR_REGION    # Null region created - deleted.

//----- SUBTRACT (Acis)
ACIS_SUBTRACT_CMD           # SUBTRACT
ACIS_SUBTRACT_ERR_ENTITIES  # At least 2 solids or coplanar regions must be selected.
ACIS_SUBTRACT_ERR_ENTITIESX # At least 2 coplanar regions must be selected.
ACIS_SUBTRACT_MSG_SELECT_A  # Select solids and regions to subtract from ...
ACIS_SUBTRACT_MSG_SELECT_AX # Select regions to subtract from ...
ACIS_SUBTRACT_MSG_SELECT_B  # Select solids and regions to subtract ...
ACIS_SUBTRACT_MSG_SELECT_BX # Select regions to subtract ...

//----- XOPEN
XOPEN_CMD
XOPEN_ERR_NOXREF      # Object is not an Xref.
XOPEN_ERR_NOSELECTION # No XRef selected.

//----- AI_3D
AI_3D_CMD             # 3D
AI_3D_MSG1            # Enter an option
AI_3D_MSG2            # [Box/Cone/DIsh/DOme/Mesh/Pyramid/Sphere/Torus/Wedge]
AI_3D_KEYS            # Box Cone DIsh DOme Mesh Pyramid Sphere Torus Wedge

//----- AI_BOX
AI_BOX_CMD            # AI_BOX
AI_BOX_MSG_CORNER     # Specify corner point of box
AI_BOX_MSG_LENGTH     # Specify length of box
AI_BOX_MSG_WIDTH      # Specify width of box or [Cube]
AI_BOX_MSG_HEIGHT     # Specify height of box
AI_BOX_KEY_CUBE       # Cube
AI_BOX_MSG_ROTATE     # Specify rotation angle of box about the Z axis or [Reference]

//----- AI_SPHERE
AI_SPHERE_CMD         # AI_SPHERE
AI_SPHERE_MSG_CENTER  # Specify center point of sphere
AI_SPHERE_MSG_RADIUS  # Specify radius of sphere or [Diameter]
AI_SPHERE_MSG_DIAM    # Specify diameter of sphere
AI_SPHERE_MSG_SURF1   # Enter number of longitudinal segments for surface of sphere <16>
AI_SPHERE_MSG_SURF2   # Enter number of latitudinal segments for surface of sphere <16>
AI_SPHERE_KEY_DIAM    # Diameter

//----- AI_DOME
AI_DOME_CMD           # AI_DOME
AI_DOME_MSG_CENTER    # Specify center point of dome
AI_DOME_MSG_RADIUS    # Specify radius of dome or [Diameter]
AI_DOME_MSG_DIAM      # Specify diameter of dome
AI_DOME_MSG_SURF1     # Enter number of longitudinal segments for surface of dome <16>
AI_DOME_MSG_SURF2     # Enter number of latitudinal segments for surface of dome <8>
AI_DOME_KEY_DIAM      # Diameter

//----- AI_DISH
AI_DISH_CMD           # AI_DISH
AI_DISH_MSG_CENTER    # Specify center point of dish
AI_DISH_MSG_RADIUS    # Specify radius of dish or [Diameter]
AI_DISH_MSG_DIAM      # Specify diameter of dish
AI_DISH_MSG_SURF1     # Enter number of longitudinal segments for surface of dish <16>
AI_DISH_MSG_SURF2     # Enter number of latitudinal segments for surface of dish <8>
AI_DISH_KEY_DIAM      # Diameter

//----- AI_CONE
AI_CONE_CMD           # AI_CONE
AI_CONE_MSG_CENTER    # Specify center point for base of cone
AI_CONE_MSG_BASERAD   # Specify radius for base of cone or [Diameter] <0>
AI_CONE_MSG_BASEDIA   # Specify diameter for base of cone <0>
AI_CONE_MSG_TOPRAD    # Specify radius for top of cone or [Diameter] <0>
AI_CONE_MSG_TOPDIA    # Specify diameter for top of cone <0>
AI_CONE_MSG_HEIGHT    # Specify height of cone
AI_CONE_MSG_SURF1     # Enter number of segments for surface of cone <16>
AI_CONE_KEY_DIAM      # Diameter
AI_CONE_ERR_2DIAM0    # Both base and top radius/diameters can not be zero.

//----- AI_MESH
AI_MESH_CMD           # AI_MESH
AI_MESH_MSG_POINT1    # Specify first corner point of mesh
AI_MESH_MSG_POINT2    # Specify second corner point of mesh
AI_MESH_MSG_POINT3    # Specify third corner point of mesh
AI_MESH_MSG_POINT4    # Specify fourth corner point of mesh
AI_MESH_MSG_SURFTAB1  # Enter mesh size in the M direction
AI_MESH_MSG_SURFTAB2  # Enter mesh size in the N direction

//----- AI_PYRAMID
AI_PYRAMID_CMD        # AI_PYRAMID
AI_PYRAMID_MSG_PT1    # Specify first corner point for base of pyramid
AI_PYRAMID_MSG_PT2    # Specify second corner point for base of pyramid
AI_PYRAMID_MSG_PT3    # Specify third corner point for base of pyramid
AI_PYRAMID_MSG_PT4    # Specify fourth corner point for base of pyramid or [Tetrahedron]
AI_PYRAMID_MSG_PT5_TETRA   # Specify apex point of tetrahedron or [Top]
AI_PYRAMID_MSG_PT5_PYRAM   # Specify apex point of pyramid or [Ridge/Top]
AI_PYRAMID_MSG_PTOP1_TETRA # Specify first corner point for top of tetrahedron
AI_PYRAMID_MSG_PTOP2_TETRA # Specify second corner point for top of tetrahedron
AI_PYRAMID_MSG_PTOP3_TETRA # Specify third corner point for top of tetrahedron
AI_PYRAMID_MSG_PTOP4_TETRA # Specify fourth corner point for top of tetrahedron
AI_PYRAMID_MSG_PTOP1_PYRAM # Specify first corner point for top of pyramid
AI_PYRAMID_MSG_PTOP2_PYRAM # Specify second corner point for top of pyramid
AI_PYRAMID_MSG_PTOP3_PYRAM # Specify third corner point for top of pyramid
AI_PYRAMID_MSG_PTOP4_PYRAM # Specify fourth corner point for top of pyramid
AI_PYRAMID_MSG_PTOP1_RIDGE # Specify first ridge end point of pyramid
AI_PYRAMID_MSG_PTOP2_RIDGE # Specify second ridge end point of pyramid
AI_PYRAMID_KEY_PYRAM  # Pyramid
AI_PYRAMID_KEY_TETRA  # Tetrahedron
AI_PYRAMID_KEY_TOP    # Top
AI_PYRAMID_KEY_RIDGE  # Ridge

//----- AI_TORUS
AI_TORUS_CMD          # AI_TORUS
AI_TORUS_MSG_CENTER   # Specify center point of torus
AI_TORUS_MSG_RADTOR   # Specify radius of torus or [Diameter]
AI_TORUS_MSG_DIATOR   # Specify diameter of torus
AI_TORUS_MSG_RADTUBE  # Specify radius of tube or [Diameter]
AI_TORUS_MSG_DIATUBE  # Specify diameter of tube
AI_TORUS_MSG_SURF1    # Enter number of segments around tube circumference <16>
AI_TORUS_MSG_SURF2    # Enter number of segments around torus circumference <16>
AI_TORUS_KEY_DIAM     # Diameter
AI_TORUS_ERR_DIAM     # Tube diameter cannot exceed torus radius.

//----- AI_WEDGE
AI_WEDGE_CMD          # AI_WEDGE
AI_WEDGE_MSG_CORNER   # Specify corner point of wedge
AI_WEDGE_MSG_LENGTH   # Specify length of wedge
AI_WEDGE_MSG_WIDTH    # Specify width of wedge
AI_WEDGE_MSG_HEIGHT   # Specify height of wedge
AI_WEDGE_MSG_ROTATE   # Specify rotation angle of wedge about the Z axis or [Reference]

//----- SYSTEM VARIABLES
SYSVAR_Msg_RO             # (read only)
SYSVAR_Msg_Invalid        # *Invalid*

SYSVAR_String_Msg       # Enter new value for %s <"%s">:
SYSVAR_String_MsgNoDef  # Enter new value for %s or %s for none <"%s">:
SYSVAR_String_Err3      # Cannot set %s to that value.

SYSVAR_Integer_Msg      # Enter new value for %s <%d>:
SYSVAR_Integer_Err1     # Requires %d or %d only.
SYSVAR_Integer_Err2     # Requires an integer between %d and %d.
SYSVAR_Integer_Err3     # Cannot set %s to that value.
SYSVAR_Integer_Err4     # Required values for

SYSVAR_Real_Msg         # Enter new value for %s <%s>:
SYSVAR_Real_Err1        # Requires an real between %f and %f.
SYSVAR_Real_Err3        # Cannot set %s to that value.

SYSVAR_Point_Msg        # Enter new value for %s <%s>:
SYSVAR_Point_Err3       # Cannot set %s to that value.


// ================= GERMAN MESSAGES ===================
[407]

//----- GENERAL
GENERAL_MSG_OVERWRITE # ist bereits vorhanden, ersetzen? [Ja/Nein] <N>
GENERAL_KEY_OVERWRITE # Ja Nein
GENERAL_ERR_FILEFOUND # Kann Datei nicht finden im Suchpfad.
GENERAL_MSG_CURWIRE   # Aktuelle Dichte des Drahtmodells
POINT_OR_KEYWORD      # Punkt oder Optionstitel wird benötigt.
NO_OBJECT_FOUND       # Kein Objekt gefunden.
ERR_NUMBER_BETWEEN    # Wert muß eine Ganzzahl zwischen %d und %d sein.
ERR_NUMBER_SEGMENTS   # Segmentanzahl muß größer als 1 sein.
ERR_TRYAGAIN          # Bitte versuchen Sie es erneut.

//------ LAYER
LAYER_ENTER_NAME      # Neuen Layernamen eingeben <%s>
INVALID_LAYER_NAME    # Ungültiger Layer Name.

//------ COLOR
COLOR_KEY_TC          # Truecolor
COLOR_KEY_TC_LOC      # Truecolor
COLOR_KEY_CB          # COlorbook
COLOR_KEY_CB_LOC      # Farbbuch
COLOR_ERR_WRONG_NR    # Ungültige Farbnummer.
COLOR_ERR_WRONG_COL   # Eine Farbnummer oder ein Standardfarbname ist erforderlich.
COLOR_ERR_WRONG_RGB   # Ungültige Farbe. Geben Sie drei durch Kommata getrennte Werte von 0 bis 255 ein.
COLOR_ERR_WRONG_CB    # Farbbuch nicht gefunden.
COLOR_ERR_WRONG_CN    # Farbe nicht gefunden.
COLOR_MSG_NEWCOLOR    # Neue Farbe
COLOR_MSG_RGB         # Rot,Grün,Blau
COLOR_MSG_COLORBOOK   # Farbbuchnamen eingeben
COLOR_MSG_COLORNAME   # Farbnamen eingeben

//=============== NEW V1.9 =============

//------ COLOR NAMES
NAME_BYCOLOR          # VonFarbe
NAME_BYBLOCK          # VonBlock
NAME_BYLAYER          # VonLayer
COLORNAME_COL_1       # Rot
COLORNAME_COL_2       # Gelb
COLORNAME_COL_3       # Grün
COLORNAME_COL_4       # Cyan
COLORNAME_COL_5       # Blau
COLORNAME_COL_6       # Magenta
COLORNAME_COL_7       # Weiß
COLORNAME_COL_BYBLOCK # VonBlock
COLORNAME_COL_BYLAYER # VonLayer
COLORNAME_COL_PREFIX  # Farbe
COLORNAME_COL_OTHERS  # Farbe wählen ...

//------ COMMON DIALOG MESSAGES
DIALOG_BTN_APPLY         # Anwenden
DIALOG_BTN_OK            # OK
DIALOG_BTN_CANCEL        # Abbrechen
DIALOG_BTN_HELP          # Hilfe
DIALOG_ERR_NOT_NUMERIC   # Eingabe ist kein numerischer Wert !

//----- MVIEW / -VPORTS
MVIEW_CMD                 # MANSFEN
VPORTS_CMD                # AFENSTER
-VPORTS_CMD               # -AFENSTER
MVIEW_MSG_MAIN            # Ecke des Ansichtsfensters angeben oder
MVIEW_KEY_ON              # Ein
MVIEW_KEY_OFF             # Aus
MVIEW_KEY_FIT             # Zbereich
MVIEW_KEY_SHADEPLOT       # SChattplot
MVIEW_KEY_HIDEPLOT        # Verdplot
MVIEW_KEY_LOCK            # speRren
MVIEW_KEY_OBJECT          # Objekt
MVIEW_KEY_POLYGONAL       # Polygonal
MVIEW_KEY_RESTORE         # Holen
MVIEW_KEY_HORIZONTAL      # Horizontal
MVIEW_KEY_VERTICAL        # Vertikal
MVIEW_KEY_ABOVE           # Über
MVIEW_KEY_BELOW           # Unter
MVIEW_KEY_LEFT            # Links
MVIEW_KEY_RIGHT           # Rechts
MVIEW_KEY_SHADE_ASIS      # Wie angezeigt
MVIEW_KEY_SHADE_WIRE      # Drahtkörper
MVIEW_KEY_SHADE_HIDDEN    # Verdeckt
MVIEW_KEY_SHADE_RENDERED  # Gerendert
MVIEW_MSG_PSPACE          # Wechselt in Papierbereich.
MVIEW_MSG_MSPACE          # Wechselt in Modellbereich.
MVIEW_MSG_OBJECT          # Objekt zum Zuschneiden des Ansichtsfensters wählen
MVIEW_MSG_SHADEPLOT       # Schattierungsplot ?
MVIEW_MSG_HIDEPLOT        # Entfernen von verdeckten Linien zum Plotten
MVIEW_MSG_LOCKVPORT       # Sperrung der Ansicht im Ansichtsfenster
MVIEW_MSG_FIRSTCORNER     # Erste Ecke angeben oder
MVIEW_MSG_OTHERCORNER     # Entgegengesetzte Ecke angeben
MVIEW_MSG_ARRANGEMENT     # Ansichtsfensteranordnung angeben
MVIEW_MSG_RESTORE         # Namen der Ansichtsfensterkonfiguration eingeben oder [?]
MVIEW_MSG_RESTORENAMES    # Einen oder mehrere Namen der aufzulistenden Ansichtsfenster-Konfiguration(en) eingeben <*>
MVIEW_MSG_CONFIGURATION   # Konfiguration
MVIEW_ERR_NOOBJECTFOUND   # Kein Objekt gefunden.
MVIEW_ERR_INVALIDOBJECT   # Ungültiges Ansichtsfenster-Schnittobjekt.
MVIEW_ERR_OBJECTNOTCLOSED # Schnittobjekt muß geschlossen sein.
MVIEW_ERR_OBJECTNOTVPORT  # war(en) kein(e) Ansichtsfenster.
MVIEW_ERR_NOVPORTCFGFOUND # Ansichtsfenster-Konfiguration nicht gefunden

//----- TrueColor Dialog
DLG_SELECTCOLOR_TITLE              # Farbe wählen
DLG_SELECTCOLOR_PAGE_ACI_TITLE     # Indexfarbe
DLG_SELECTCOLOR_PAGE_ACI_MSG       # AutoCAD-Farbindex (ACI)
DLG_SELECTCOLOR_PAGE_ACI_MSG1      # Indexfarbe
DLG_SELECTCOLOR_PAGE_ACI_MSG2      # Rot, Grün, Blau
DLG_SELECTCOLOR_PAGE_RGB_TITLE     # True Color
DLG_SELECTCOLOR_PAGE_RGB_SPACE     # Bereich
DLG_SELECTCOLOR_PAGE_RGB_WEB       # Web
DLG_SELECTCOLOR_PAGE_CBK_TITLE     # Farbbücher
DLG_SELECTCOLOR_PAGE_CBK_MSG       # Farbbuch :
DLG_SELECTCOLOR_PAGE_CBK_SORT      # Farben sortieren
DLG_SELECTCOLOR_PAGE_CBK_FOLDER    # Farbbuch-Speicherort
DLG_SELECTCOLOR_PAGE_CBK_SELECT    # Wählen Sie das Farbbücher-Verzeichnis
DLG_SELECTCOLOR_PAGE_CBK_MSG1      # Wichtiger Hinweis
DLG_SELECTCOLOR_PAGE_CBK_MSG2      # Das neue Farbbücher-Verzeichnis ist erst nach Neustart von AutoCAD LT aktiv !
DLG_SELECTCOLOR_PAGE_CBK_CUSTOM    # Wie erstelle ich Farbbücher ?
DLG_SELECTCOLOR_PAGE_CBK_ACBHELP   # Custom-ACB.Deutsch.rtf
DLG_SELECTCOLOR_PAGE_CBK_NOACBRTF  # Die Dokumentation zur Farbbuch-Erstellung ist nicht vorhanden !
DLG_SELECTCOLOR_PAGE_CBK_NOWORDPAD # Die "WordPad.exe" Programmdatei kann nicht gefunden werden !

//----- MLINE STYLE
MLSTYLE_CMD                 # MLSTIL
MLSTYLE_DLG_TITLE           # Multilinien Stile
MLSTYLE_DLG_MSG_CURR        # Aktuell
MLSTYLE_DLG_MSG_DESC        # Beschreibung
MLSTYLE_DLG_BTN_LOAD        # Laden
MLSTYLE_DLG_BTN_SAVE        # Speichern
MLSTYLE_DLG_BTN_ADD         # Hinzu
MLSTYLE_DLG_BTN_RENAME      # Umbenenn
MLSTYLE_DLG_BTN_DELETE      # Löschen
MLSTYLE_DLG_PAGE_ELEMENTS   # Element-Eigenschaften
MLSTYLE_DLG_MSG_OFFSET      # Abstand
MLSTYLE_DLG_MSG_COLOR       # Farbe
MLSTYLE_DLG_MSG_LTYPE       # Linientyp
MLSTYLE_DLG_PAGE_MLPROPS    # Multilinien-Eigenschaften
MLSTYLE_DLG_CHK_JOINTS      # Verbindung zeigen
MLSTYLE_DLG_CHK_FILL        # Füllung
MLSTYLE_DLG_FRM_CAPS        # Abschlußstücke
MLSTYLE_DLG_MSG_LINE        # Linie
MLSTYLE_DLG_MSG_OARC        # Bogen außen
MLSTYLE_DLG_MSG_IARC        # Bogen innen
MLSTYLE_DLG_MSG_ANGLE       # Winkel
MLSTYLE_DLG_MSG_ATSTART     # am Anfang
MLSTYLE_DLG_MSG_ATEND       # am Ende
MLSTYLE_DLG_MSG_ENTITIES    # Anzahl Multilinien mit diesem Stil
MLSTYLE_DLG_MSG_DELETE_1    # Element löschen ?
MLSTYLE_DLG_MSG_DELETE_2    # Möchten Sie das gewählte Element löschen ?
MLSTYLE_DLG_SAVEFILE_TITLE  # Aktuellen Multilinien-Stil speichern
MLSTYLE_DLG_LOADFILE_TITLE  # Multilinien-Stil laden
MLSTYLE_DLG_LOADFILE_FILE   # Datei...
MLSTYLE_DLG_LOADFILE_FMSG   # Wahl der Multilinien-Stil-Datei

//----- MLINE
MLINE_CMD          # MLINIE
MLINE_MSG_CURR     # Aktuelle Einstellungen
MLINE_MSG_MAIN     # Startpunkt angeben oder [Ausrichtung/Maßstab/Stil]
MLINE_KEY_MAIN     # Ausrichtung Maßstab Stil
MLINE_KEY_JUST     # Ausrichtung
MLINE_KEY_SCALE    # Maßstab
MLINE_KEY_STYLE    # Stil
MLINE_JUST_TOP     # Oben
MLINE_JUST_ZERO    # Null
MLINE_JUST_BOTTOM  # Unten
MLINE_MSG_JUST     # Ausrichtungstyp eingeben [Oben/Null/Unten]
MLINE_MSG_SCALE    # Mlinienmaßstab eingeben
MLINE_MSG_STYLE    # Mlinienstilnamen eingeben oder [?]
MLINE_MSG_STLIST   # Name                               Beschreibung
MLINE_MSG_FILEDLG  # Multilinienstil aus Datei laden
MLINE_ERR_NOTFOUND # Multilinienstil <%s> nicht gefunden in Datei
MLINE_MSG_PTNEXT1  # Nächsten Punkt angeben
MLINE_MSG_PTNEXT2  # Nächsten Punkt angeben oder
MLINE_KEY_UNDO     # Zurück
MLINE_KEY_CLOSE    # Schließen

//----- -REFEDIT
REFEDITCMDLINE_CMD       # -REFEDIT
REFEDITCMDLINE_MSG_MAIN  # Verschachtelungsebene auswählen [OK/Nächste] <Nächste>
REFEDITCMDLINE_KWD_MAIN  # OK Nächste
REFEDITCMDLINE_MSG_ATTS  # Attributdefinitionen anzeigen [Ja/Nein] <Nein>
REFEDITCMDLINE_KWD_ATTS  # Ja Nein
REFEDITCMDLINE_MSG_MODE  # Objektauswahlverfahren eingeben [Alle/Eingebettet] <Alle>
REFEDITCMDLINE_KWD_MODE  # Alle Eingebettet

//----- REFEDIT
REFEDIT_CMD              # REFBEARB
REFEDIT_MSG_SELECT       # XRef/Block Reference wählen
REFEDIT_MSG_NOTICE       # Verwenden Sie REFSCHLIESSEN oder den Werkzeugkasten 'RefBearbeiten', um die Referenzbearbeitungs-Sitzung zu schliessen.
REFEDIT_MSG_RELOAD       # XRef ist nicht aktuell - XRef Zeichnung wurde neu geladen.
REFEDIT_MSG_NRITEMSADDED # Elemente wurdem dem Referenz-Satz zugefügt.
REFEDIT_ERR_INPROGRESS   # ** Befehl nicht zulässig, ein XRef/Block ist bereits zur Bearbeitung entnommen **
REFEDIT_ERR_NOXEDIT      # Dieses XRef kan nicht 'In-Place' bearbeitet werden : XEDIT=0 ist gesetzt !
REFEDIT_ERR_DWGLOCKED1   # XRef kann nicht zur Bearbeitung entnommen werden.
REFEDIT_ERR_DWGLOCKED2   # Die Zeichnungsdatei ist bereits gesperrt.
REFEDIT_ERR_NOREFERENCE  # Objekt ist keine gültige Referenz.
REFEDIT_ERR_NOSELECTION  # Referenz nicht gefunden.
REFEDIT_ERR_NOENTITIES   # Keine Elemente für Referenz-Satz gewählt.
REFEDIT_ERR_NO_SAVE      # Befehl ist während der Referenz-Bearbeitung nicht möglich

REFEDIT_ERR_HOSTOBJECT_1 # Die gewählten Elemente gehören zur Hauptzeichnung, nicht zum Referenz-Objekt !
REFEDIT_ERR_HOSTOBJECT_2 # Bitte seien Sie sehr sorgfältig bei dieser Vorgehensweise !

//----- REFEDIT DIALOG
REFEDIT_DLG_TITLE        # Referenz bearbeiten
REFEDIT_DLG_PAGE_REFS    # Referenz identifizieren
REFEDIT_DLG_REFNAME      # Referenzname
REFEDIT_DLG_PREVIEW      # Voransicht
REFEDIT_DLG_PATH         # Pfad
REFEDIT_DLG_AUTO         # Alle eingebetteten Objekte automatisch wählen
REFEDIT_DLG_MANU         # Aufforderung, eingebettete Objekte manuell zu wählen
REFEDIT_DLG_TIP_AUTO     # Mit dieser Option werden alle Elemente der gewählten Referenz automatisch gewählt
REFEDIT_DLG_TIP_MANU     # Es erscheint die Elementauswahl-Anfrage, um eingebettete Elemente manuell zu wählen
REFEDIT_DLG_PAGE_OPTS    # Einstellungen
REFEDIT_DLG_UNIQUE       # Eindeutige Layer-, Stil- und Blocknamen erstellen
REFEDIT_DLG_ATTRIBUTES   # Attributdefinitionen für Bearbeitung anzeigen
REFEDIT_DLG_LOCK         # Objekte sperren, die nicht im Referenz-Satz enthalten sind
REFEDIT_DLG_HIDESELECT   # Bei Objektwahl alle Objekte ausblenden, die nicht im Referenz-Satz enthalten sind
REFEDIT_DLG_HIDEEDIT     # Bei Referenz-Bearbeitung alle Objekte ausblenden, die nicht im Referenz-Satz enthalten sind
REFEDIT_DLG_FILTER       # Objekt-Filter verwenden
REFEDIT_DLG_SETFILTER    # Filter einstellen ...
REFEDIT_DLG_TIP_UNIQUE   # Steuert, ob Layer, Blöcke, Stile und andere Tabellenobjekte, welche aus der Referenz extrahiert werden, eindeutig benannt werden (entspricht BINDTYPE)
REFEDIT_DLG_TIP_ATTRIBS  # Steuert, ob alle variablen Attribut-Definitionen ebenfalls aus der Referenz extrahiert und für die Bearbeitung angezeigt werden sollen.
REFEDIT_DLG_TIP_LOCK     # Sperrt alle Objekte, welche nicht zum Referenz-Satz gehören. Damit wird verhindert, dass Sie versehentlich Elemente aus der Hauptzeichnug wählen und bearbeiten, während Sie sich im Referenz-Edit-Modus befinden
REFEDIT_DLG_TIP_HIDESEL  # Blendet alle Objekte der Hauptzeichnung aus, wenn die Elemente der zu bearbeitenden Referenz gewählt werden
REFEDIT_DLG_TIP_HIDEEDT  # Blendet alle Objekte der Hauptzeichnung aus, während Sie sich im Modus der Referenz-Bearbeitung befinden (kann in RefEdit-Toolbar jederzeit Ein/Aus geschaltet werden)
REFEDIT_DLG_TIP_FILTER   # Aktiviert den Element-Filter, um nur bestimmte Elemente in den Arbeitssatz zu übernehmen
REFEDIT_DLG_TIP_SETFILTER# Definiert den Element-Filter ...

REFEDIT_DLG_FILTER_TITLE # Objekt-Filter
REFEDIT_FLT_BTN_ALL      # Alle
REFEDIT_FLT_BTN_NONE     # Keine
REFEDIT_FLT_POINT        # Punkt
REFEDIT_FLT_LINE         # Linie
REFEDIT_FLT_MLINE        # MLinie
REFEDIT_FLT_POLYLINE     # Polylinie
REFEDIT_FLT_SPLINE       # Spline
REFEDIT_FLT_ARC          # Bogen
REFEDIT_FLT_CIRCLE       # Kreis
REFEDIT_FLT_ELLIPSE      # Ellipse
REFEDIT_FLT_3DFACE       # 3D-Fläche
REFEDIT_FLT_SOLID        # Solid-Fläche
REFEDIT_FLT_INSERT       # Block/XRef
REFEDIT_FLT_HATCH        # Schraffur
REFEDIT_FLT_IMAGE        # Bild
REFEDIT_FLT_SHAPE        # Symbol
REFEDIT_FLT_TRACE        # Band
REFEDIT_FLT_VIEWPORT     # Viewport
REFEDIT_FLT_TEXT         # Text
REFEDIT_FLT_MTEXT        # MText
REFEDIT_FLT_LEADER       # Führungslinie
REFEDIT_FLT_DIMENSION    # Bemassung
REFEDIT_FLT_REGION       # Region
REFEDIT_FLT_3DSOLID      # 3D Volumenkörper
REFEDIT_FLT_MPOLYGON     # MPolygon
REFEDIT_FLT_OTHERS       # Sonstige

//----- REFEDIT TOOLBAR
REFEDIT_TB_TITLE         # Referenz bearbeiten
REFEDIT_TB_VISIBLE       # Anzeigen oder Ausblenden der Objekte ausserhalb des Referenz-Satzes
REFEDIT_TB_TIP_NAME      # Zeigt die aktuell in Bearbeitung befindliche Referenz
REFEDIT_TB_TIP_EDIT      # Referenz-Bearbeitung eines XRefs/Blocks
REFEDIT_TB_TIP_ADD       # Objekte dem Bearbeitungs-Satz hinzufügen
REFEDIT_TB_TIP_REM       # Objekte aus dem Bearbeitungs-Satz entfernen
REFEDIT_TB_TIP_DISCARD   # Änderungen an Referenz verwerfen
REFEDIT_TB_TIP_SAVE      # Änderungen an Referenz speichern
REFEDIT_TB_TIP_XFADE     # Stellt die Ausblend-Intensität (XFadeCtl) für Objekte ausserhalb des Referenz-Satzes ein

//----- REFSET
REFSET_CMD               # REFSATZ
REFSET_MSG_INFO          # Objekte zwischen Referenz-Arbeitssatz und Host-Zeichnung übertragen ...
REFSET_MSG_ADDREM        # Option eingeben [Hinzufügen/Entfernen] <Hinzufügen>
REFSET_KWD_ADDREM        # Hinzufügen Entfernen
REFSET_MSG_NRITEMSADD    # Objekt(e) wurden dem Referenz-Arbeitssatz hinzugefügt.
REFSET_MSG_NRITEMSREM    # Objekt(e) wurden aus dem Referenz-Arbeitssatz entfernt.
REFSET_ERR_NOREFEDIT     # ** Befehl nur zulässig, wenn eine Referenz mit dem Befehl REFBEARB entnommen wird **

//----- REFCLOSE
REFCLOSE_CMD             # REFSCHLIESSEN
REFCLOSE_MSG_CLOSE       # Option eingeben [Speichern/Verwerfen] <Speichern>
REFCLOSE_KWD_CLOSE       # Speichern Verwerfen
REFCLOSE_MSG_SAVE_1      # Alle Änderungen an den Referenzen werden gespeichert.
REFCLOSE_MSG_SAVE_2      # - Um die Änderung der Referenzen zu speichern, klicken Sie <OK>.
REFCLOSE_MSG_SAVE_3      # - Um den Befehl abzubrechen, klicken Sie <ABBRECHEN>.
REFCLOSE_MSG_DISCARD_1   # Alle Änderungen an den Referenzen werden verworfen.
REFCLOSE_MSG_DISCARD_2   # - Um die Änderung der Referenzen zu verwerfen, klicken Sie <OK>.
REFCLOSE_MSG_DISCARD_3   # - Um den Befehl abzubrechen, klicken Sie <ABBRECHEN>.
REFCLOSE_ERR_NOREFEDIT   # ** Befehl nur zulässig, wenn eine Referenz mit dem Befehl REFBEARB entnommen wird **

//----- SCRIPT
SCRIPT_CMD             # SCRIPT
SCRIPT_MSG_SELECT      # Scriptdateinamen eingeben
SCRIPT_DLG_SELECT      # Skriptdatei auswählen
SCRIPT_DLG_FILETYPE    # Skript Dateien (*.scr)
SCRIPT_ERR_NOFILEFOUND # Kann die Datei nicht finden

//------ PROFILE PAGE OPTIONS DIALOG
PROFILE_PAGE_MSG_PROFILES  # Profile
PROFILE_PAGE_MSG_CURRENT   # Aktuelles Profil :
PROFILE_PAGE_BTN_CURRENT   # Aktuell
PROFILE_PAGE_BTN_NEW       # Neu
PROFILE_PAGE_BTN_COPY      # Kopieren
PROFILE_PAGE_BTN_RENAME    # Umbenennen
PROFILE_PAGE_BTN_DELETE    # Löschen
PROFILE_PAGE_BTN_EXPORT    # Exportieren
PROFILE_PAGE_BTN_IMPORT    # Importieren
PROFILE_PAGE_BTN_RESET     # Zurücksetzen
PROFILE_PAGE_MSG_DELETE    # Sind Sie sicher, dass Sie dieses Profil löschen möchten ?
PROFILE_PAGE_MSG_NODELETE  # Das aktuelle Profil kann nicht gelöscht werden !
PROFILE_PAGE_MSG_RESET     # Sind Sie sicher, dass Sie dieses Profil zurücksetzen möchten ?

PROFILE_PAGE_TITLE_ADD     # Profil hinzufügen
PROFILE_PAGE_TITLE_COPY    # Profil kopieren
PROFILE_PAGE_TITLE_RENAME  # Profil ändern
PROFILE_PAGE_TITLE_EXPORT  # Profil exportieren
PROFILE_PAGE_TITLE_IMPORT  # Profil importieren
PROFILE_PAGE_MSG_NAME      # Profil Name
PROFILE_PAGE_MSG_DESC      # Beschreibung
PROFILE_PAGE_MSG_PATH      # Include Path information
PROFILE_PAGE_MSG_FILETYPE  # Profile (*.arg)|arg

PROFILE_DLG_EDIT_MSG_NAME  # Profilname
PROFILE_DLG_EDIT_MSG_DESC  # Beschreibung
PROFILE_DLG_EDIT_BTN_PATH  # Pfadinformationen einbeziehen
PROFILE_DLG_EDIT_BTN_OK    # Anwenden && Schliessen
PROFILE_DLG_EDIT_BTN_QUIT  # Abbrechen

//------ 3D ORBIT CONTEXT MENU
3DORBIT_MENU_EXIT        # Beenden
3DORBIT_MENU_SHADINGMODE # Schattierungsmodi
3DORBIT_MENU_PROJECTION  # Projektion
3DORBIT_MENU_CLIPPING    # Zuschneiden
3DORBIT_MENU_OTHERS      # Sonstiges
3DORBIT_MENU_PAN         # Pan
3DORBIT_MENU_ZOOM        # Zoom
3DORBIT_MENU_ORBIT       # Orbit
3DORBIT_MENU_CORBIT      # Kont. Orbit
3DORBIT_MENU_DISTANCE    # 3D-Abstand
3DORBIT_MENU_SWIVEL      # 3D-Schwenken
3DORBIT_MENU_FRONTCLIP   # Vorderes Zuschneiden Ein
3DORBIT_MENU_BACKCLIP    # Hinteres Zuschneiden Ein
3DORBIT_MENU_CLIPDLG     # Zuschneide-Ebenen Anpassen
3DORBIT_MENU_PARALLEL    # Parallel
3DORBIT_MENU_PERSPECTIVE # Perspektive
3DORBIT_MENU_REFERENCEPT # Pick Orbit-Zentrum
3DORBIT_MENU_GETDWGVIEW  # Lade Ansicht aus DWG
3DORBIT_MENU_SETDWGVIEW  # Setze Ansicht in DWG
3DORBIT_MENU_3DCFG       # 3D Konfiguration

//------ 3D ORBIT
3DORBIT_CMD              # 3DORBIT
3DORBIT_MSG_1            # Mit ESC oder EINGABETASTE beenden oder rechte Maustaste für Kontextmenü
3DORBIT_MSG_2            # Links+Maus = ORBIT, Shift-Links+Maus = PAN, Strg-Links+Maus = ZOOM.

//------ 3D CONT ORBIT
3DCORBIT_CMD             # 3DORBITFORTL
3DCORBIT_MSG_1           # Mit ESC oder EINGABETASTE beenden oder rechte Maustaste für Kontextmenü
3DCORBIT_MSG_2           # Links+Maus = KONT.ORBIT, Shift-Links+Maus = PAN, Strg-Links+Maus = ZOOM.

//------ 3D ORBIT CENTER
3DORBITCTR_CMD           # 3DORBITCTR
3DORBITCTR_MSG_0         # Orbit-Mittelpunkt festlegen :
3DORBITCTR_MSG_1         # Mit ESC oder EINGABETASTE beenden oder rechte Maustaste für Kontextmenü
3DORBITCTR_MSG_2         # Links+Maus = KONT.ORBIT, Shift-Links+Maus = PAN, Strg-Links+Maus = ZOOM.

//------ 3D DISTANCE
3DDISTANCE_CMD           # 3DENTFERNUNG
3DDISTANCE_MSG_1         # Mit ESC oder EINGABETASTE beenden oder rechte Maustaste für Kontextmenü
3DDISTANCE_MSG_2         # Links+Maus = 3D-ENTFERNUNG, Shift-Links+Maus = PAN, Strg-Links+Maus = ZOOM.

//------ 3D SWIVEL
3DSWIVEL_CMD             # 3DSCHWENKEN
3DSWIVEL_MSG_1           # Mit ESC oder EINGABETASTE beenden oder rechte Maustaste für Kontextmenü
3DSWIVEL_MSG_2           # Links+Maus = 3D-SCHWENKEN, Shift-Links+Maus = PAN, Strg-Links+Maus = ZOOM.

//------ 3D CAMERA
3DCAMERA_CMD             # KAMERA
3DCAMERA_MSG_CAMERA      # Neue Kameraposition angeben
3DCAMERA_MSG_TARGET      # Neues Kameraziel angeben

//------ 3D CLIP
3DCLIP_CMD               # 3DSCHNITT
3DCLIP_DIALOG_TITLE      # Zuschneideebenen anpassen
3DCLIP_MSG               # Mit ESC oder EINGABETASTE beenden oder rechte Maustaste für Kontextmenü
3DCLIP_BTN_CLOSE         # Schliessen
3DCLIP_TIP_ADJUSTFRONT   # Vorderen Schnitt anpassen
3DCLIP_TIP_ADJUSTBACK    # Hinteren Schnitt anpassen
3DCLIP_TIP_ADJUSTSLICE   # Vorderen und Hinteren Schnitt anpassen
3DCLIP_TIP_FRONTONOFF    # Aktiviert vordere Schnittebene (Ein/Aus)
3DCLIP_TIP_BACKONOFF     # Aktiviert hintere Schnittebene (Ein/Aus)
3DCLIP_TIP_EDITFRONT     # Vordere Schnittebene manuell definieren
3DCLIP_TIP_EDITBACK      # Hintere Schnittebene manuell definieren
3DCLIP_TIP_VIEWAREA_PAN  # Maus im 3D-View : || Links = Schnittebenen einstellen || Shift-Links = Pan || Strg-Links = Zoom

//------ 3D ORBIT VIEW
3DORBITVIEW_CMD               # 3DORBITVIEW
3DORBITVIEW_MSG_PICKREF       # Bitte den Bezugspunkt picken :
3DORBITVIEW_MODE_2D           # 2D Optimiert
3DORBITVIEW_MODE_3D           # 3D Drahtmodell
3DORBITVIEW_MODE_HIDDEN       # Verdeckt
3DORBITVIEW_MODE_FLAT         # Flat
3DORBITVIEW_MODE_GOURAUD      # Gouraud
3DORBITVIEW_MODE_FLATX        # Flat+Kanten
3DORBITVIEW_MODE_GOURAUDX     # Gouraud+Kanten
3DORBITVIEW_BTN_CLIPDLG       # 3D-Schnitt...
3DORBITVIEW_BTN_EXTRAS>       # Layer >>>
3DORBITVIEW_BTN_EXTRAS<       # Layer <<<
3DORBITVIEW_BTN_SMART         # Layer Vorschau
3DORBITVIEW_BTN_ALL           # Alle
3DORBITVIEW_BTN_NONE          # Keine
3DORBITVIEW_BTN_SEL           # Markieren
3DORBITVIEW_BTN_UNSEL         # De-Markieren
3DORBITVIEW_TIP_VIEWMODE      # Darstellungsmodus für 3D-View
3DORBITVIEW_TIP_ANGLE_XY      # horizontaler Blickwinkel in XY-Ebene
3DORBITVIEW_TIP_ANGLE_Z       # vertikaler Blickwinkel über XY-Ebene
3DORBITVIEW_TIP_GETDWG_VIEW   # Zeichnungs-Ansicht in den 3D-Orbit übertragen
3DORBITVIEW_TIP_SETDWG_VIEW   # Ansicht aus dem 3D-Orbit in die Zeichnung übertragen
3DORBITVIEW_TIP_GET_REFPT     # Referenzpunkt in der Zeichnung picken
3DORBITVIEW_TIP_CFG           # 3D-Konfigurations-Dialog
3DORBITVIEW_TIP_RELOAD        # 3D-Orbit-Viewer neu aus der Zeichnung laden
3DORBITVIEW_TIP_SLIDER_HOR    # horizontalen Blickwinkel einstellen
3DORBITVIEW_TIP_SLIDER_VER    # vertikalen Blickwinkel einstellen
3DORBITVIEW_TIP_SLIDER_ZOOM   # Relativen Zoom einstellen (0.01 - 100)
3DORBITVIEW_TIP_EXTRAS        # Zeigt erweiterte Merkmale für 3D-View
3DORBITVIEW_TIP_LAYERS        # Markierte Layer werden im 3D-Orbit-Viewer angezeigt
3DORBITVIEW_TIP_LOADLAYERS    # Layer-Liste aktualisieren
3DORBITVIEW_TIP_SMARTLAYERS   # Ist die 'Layer Vorschau' aktiv, so zeigt der 3D Orbit nur die markierten Layer an; anderenfalls werden alle selektierten Layer gezeigt.
3DORBITVIEW_TIP_ALL           # Alle Layer zur Anzeige wählen
3DORBITVIEW_TIP_NONE          # Keine Layer zur Anzeige wählen
3DORBITVIEW_TIP_SEL           # Markiert alle gewählten Layer
3DORBITVIEW_TIP_UNSEL         # De-Markiert alle gewählten Layer
3DORBITVIEW_TIP_MODE_ORBIT    # Maus Modus : Orbit
3DORBITVIEW_TIP_MODE_PAN      # Maus Modus : Pan
3DORBITVIEW_TIP_MODE_ZOOM     # Maus Modus : Zoom
3DORBITVIEW_TIP_MODE_DIST     # Maus Modus : Distance
3DORBITVIEW_TIP_MODE_SWIV     # Maus Modus : Schwenken
3DORBITVIEW_TIP_MODE_CORBIT   # Fortlaufender Orbit : Ein/Aus
3DORBITVIEW_TIP_MODE_PERSP    # Projektion : Perspektive=Ein/Parallel=Aus
3DORBITVIEW_TIP_FRONTCLIP     # Vordere Schnittebene Ein/Aus
3DORBITVIEW_TIP_BACKCLIP      # Hintere Schnittebene Ein/Aus
3DORBITVIEW_TIP_CLIPDLG       # Definiert Vordere & Hintere Schnittebene
3DORBITVIEW_TIP_MODE_SAVEDISK # Speichert die Anzeige als Image-Datei

3DORBITVIEW_TIP_VIEWAREA_ORB  # Maus im 3D-View : || Links = Orbit || Shift-Links = Pan  || Strg-Links = Zoom  || Rechts = Kontext-Menu
3DORBITVIEW_TIP_VIEWAREA_PAN  # Maus im 3D-View : || Links = Pan   || Shift-Links = Zoom || Strg-Links = Orbit || Rehts  = Kontext-Menu
3DORBITVIEW_TIP_VIEWAREA_ZOOM # Maus im 3D-View : || Links = Zoom  || Shift-Links = Pan  || Strg-Links = Orbit || Rechts = Kontext-Menu
3DORBITVIEW_TIP_VIEWAREA_DIST # Maus im 3D-View : || Links = Abstand || Shift-Links = Pan || Strg-Links = Zoom || Rechts = Kontext-Menu
3DORBITVIEW_TIP_VIEWAREA_SWIV # Maus im 3D-View : || Links = Schwenken || Shift-Links = Pan || Strg-Links = Zoom || Rechts = Kontext-Menu
3DORBITVIEW_TIP_VIEWAREA_CORB # Maus im 3D-View : Fortlaufender Orbit

//------ TEXT & ATTRIBUTE
TEXTATT_JUST_BASE_LEFT   # Links
TEXTATT_JUST_BASE_ALIGN  # Ausrichten
TEXTATT_JUST_BASE_FIT    # Anpassen
TEXTATT_JUST_BASE_CENTER # Zentriert
TEXTATT_JUST_BASE_MIDDLE # Mitte
TEXTATT_JUST_BASE_RIGHT  # Rechts
TEXTATT_JUST_TOP_LEFT    # Oben links
TEXTATT_JUST_TOP_CENTER  # Oben zentriert
TEXTATT_JUST_TOP_RIGHT   # Oben rechts
TEXTATT_JUST_MID_LEFT    # Mitte links
TEXTATT_JUST_MID_CENTER  # Mitte zentriert
TEXTATT_JUST_MID_RIGHT   # Mitte rechts
TEXTATT_JUST_BOT_LEFT    # Unten links
TEXTATT_JUST_BOT_CENTER  # Unten zentriert
TEXTATT_JUST_BOT_RIGHT   # Unten rechts

//------ EATTEDIT DIALOG
EATTEDIT_CMD                # EATTEDIT
EATTEDIT_MSG_SELBLOCK       # Wählen Sie einen Block
EATTEDIT_ERR_SELBLOCK       # Fehler beim Auswählen eines Objekts.
EATTEDIT_ERR_NOBLATTS       # Diese Zeichnung enthält keine attributierten Blöcke.
EATTEDIT_ERR_NOBLOCK        # Das ausgewählte Element ist kein Block.
EATTEDIT_ERR_NOATTS         # Der ausgewählte Block besitzt keine bearbeitbaren Attribute.
//------ MAIN DIALOG PAGE
EATTEDIT_DLG_TITLE          # Erweiterter Atrribut-Editor
EATTEDIT_DLG_BLOCK          # Block
EATTEDIT_DLG_TAG            # Bezeichnung
EATTEDIT_DLG_SELECTBLOCK    # Block auswählen
//------ PAGE : ATTRIBUTE
ATTRIBUTE_PAGE_ATT_TITLE    # Attribut
ATTRIBUTE_PAGE_ATT_TAG      # Bezeichnung
ATTRIBUTE_PAGE_ATT_PROMPT   # Aufforderung
ATTRIBUTE_PAGE_ATT_VALUE    # Wert
ATTRIBUTE_PAGE_ATT_DEFAULT  # Vorgabe
ATTRIBUTE_PAGE_ATT_FRAME_1  # Modus
ATTRIBUTE_PAGE_ATT_FRAME_2  # Daten
ATTRIBUTE_PAGE_ATT_INVIS    # Unsichtbar
ATTRIBUTE_PAGE_ATT_CONSTANT # Konstant
ATTRIBUTE_PAGE_ATT_VERIFY   # Überprüfen
ATTRIBUTE_PAGE_ATT_PRESET   # Voreinstellen
//------ PAGE : TEXT
ATTRIBUTE_PAGE_TXT_TITLE    # Textoptionen
ATTRIBUTE_PAGE_TXT_STYLE    # Textstil
ATTRIBUTE_PAGE_TXT_JUSTIFY  # Ausrichtung
ATTRIBUTE_PAGE_TXT_BACKWARD # Rückwärts
ATTRIBUTE_PAGE_TXT_UPSIDE   # Auf dem Kopf
ATTRIBUTE_PAGE_TXT_HEIGHT   # Höhe
ATTRIBUTE_PAGE_TXT_WIDTH    # Breitenfaktor
ATTRIBUTE_PAGE_TXT_ROTATION # Drehung
ATTRIBUTE_PAGE_TXT_ANGLE    # Neigungswinkel
//------ PAGE : PROPERTIES
ATTRIBUTE_PAGE_PRP_TITLE    # Eigenschaften
ATTRIBUTE_PAGE_PRP_LAYER    # Layer
ATTRIBUTE_PAGE_PRP_LTYPE    # Linientyp
ATTRIBUTE_PAGE_PRP_COLOR    # Farbe
ATTRIBUTE_PAGE_PRP_LWEIGHT  # Linienstärke
ATTRIBUTE_PAGE_PRP_PSTYLE   # Plotstil

//------ BATTMAN DIALOG
BATTMAN_CMD                 # BATTMAN
//------ MAIN DIALOG PAGE
BATTMAN_DLG_TITLE           # Blockatrribut-Manager
BATTMAN_DLG_SELECTBLOCK     # Block wählen
BATTMAN_DLG_BLOCK           # Block :
BATTMAN_DLG_SYNCALL         # Synchr.
BATTMAN_DLG_SYNCSEL         # Synchr. Wahl
BATTMAN_DLG_MOVEUP          # Nach Oben
BATTMAN_DLG_MOVEDOWN        # Nach Unten
BATTMAN_DLG_EDIT            # Bearbeiten
BATTMAN_DLG_REMOVE          # Entfernen
BATTMAN_DLG_SETTINGS        # Einstellungen...
BATTMAN_MSG_SELECTINSERTS   # Wählen Sie Blockeinfügungen zum Synchronisieren
BATTMAN_MSG_FOUND           # Gefunden in Zeichnung
BATTMAN_MSG_FOUNDINMODEL    # Gefunden im Modellbereich
BATTMAN_MSG_FOUNDINLAYOUT   # Gefunden im aktuellen Layout
BATTMAN_MSG_BLOCKMODIFIED_1 # Die Attribut im aktuellen Block wurden geändert
BATTMAN_MSG_BLOCKMODIFIED_2 # Möchten Sie die Änderungen speichern ?
BATTMAN_KEY_TAG             # Bezeichnung
BATTMAN_KEY_PROMPT          # Aufforderung
BATTMAN_KEY_DEFAULT         # Vorgabe
BATTMAN_KEY_MODES           # Modi
BATTMAN_KEY_MODES_INVISIBLE # Unsichtbar
BATTMAN_KEY_MODES_CONSTANT  # Konstant
BATTMAN_KEY_MODES_PRESET    # Voreinstellen
BATTMAN_KEY_MODES_VERIFY    # Überprüfen
BATTMAN_KEY_STYLE           # Stil
BATTMAN_KEY_JUSTIFY         # Ausrichtung
BATTMAN_KEY_HEIGHT          # Höhe
BATTMAN_KEY_ROTATION        # Drehung
BATTMAN_KEY_WIDTHFACTOR     # Breitenfaktor
BATTMAN_KEY_OBLIQUE         # Neigungswinkel
BATTMAN_KEY_LAYER           # Layer
BATTMAN_KEY_LINETYPE        # Linientyp
BATTMAN_KEY_COLOR           # Farbe
BATTMAN_KEY_LINEWEIGHT      # Lineweight
BATTMAN_KEY_PLOTSTYLE       # Plotstil
BATTMAN_ERR_TAGMISSING      # Die Attribut-Bezeichnung muss angegeben werden.
//------ BATTMAN SETTINGS DIALOG
BATTMAN_SETTINGS_DLG_TITLE  # Einstellungen
BATTMAN_SETTINGS_DLG_FRAME  # In Liste anzeigen
BATTMAN_SETTINGS_DLG_ALL    # Alles wählen
BATTMAN_SETTINGS_DLG_NONE   # Alles löschen
BATTMAN_SETTINGS_DLG_EMPHAS # Doppelte Bezeichnungen hervorheben
BATTMAN_SETTINGS_DLG_APPLY  # Auf bestehende Blockreferenzen anwenden
//------ BATTMAN ATTRIBUT EDIT DIALOG
BATTMAN_ATTRIB_DLG_TITLE    # Attribut bearbeiten
BATTMAN_ATTRIB_DLG_BLOCK    # Block
BATTMAN_ATTRIB_DLG_ATTRIB   # Attribut
BATTMAN_ATTRIB_DLG_AUTOAPP  # Autom. Voransicht der Änderungen

//------ ATTSYNC
ATTSYNC_CMD           # ATTSYNC
ATTSYNC_MSG           # Option eingeben [?/Name/Auswählen] <Auswählen>
ATTSYNC_KEY           # ? Name Auswählen
ATTSYNC_SELECT        # Wählen Sie einen Block
ATTSYNC_LIST          # Attributierte Blöcke
ATTSYNC_NAME          # Name des zu synchronisierenden Blocks eingeben oder [?]
ATTSYNC_DOSYNC        # ATTSYNC block %s ? [Ja/Nein] <Ja>:
ATTSYNC_KEY_YESNO     # Ja Nein
ATTSYNC_COMPLETE      # ATTSYNC abgeschlossen.
ATTSYNC_ERR_SELBLOCK  # Fehler beim Auswählen eines Objekts.
ATTSYNC_ERR_NOBLOCK   # Das ausgewählte Element ist kein Block.
ATTSYNC_ERR_NOBLATTS  # Diese Zeichnung enthält keine attributierten Blöcke.
ATTSYNC_ERR_NOATTS    # Der ausgewählte Block besitzt keine bearbeitbaren Attribute.
ATTSYNC_ERR_WRONGNAME # Kein Block mit angegebenem Namen definiert !

//------ LAYERPMODE
LAYERPMODE_CMD        # LAYERVMODUS
LAYERPMODE_MSG        # Eingeben von LAYERV-Modus [EIN/AUS]
LAYERPMODE_KWD_ON     # EIN
LAYERPMODE_KWD_OFF    # AUS

//------ LAYERP
LAYERP_CMD            # LAYERV
LAYERP_MSG_NORM       # Status "Vorherige Layer" wiederhergestellt.
LAYERP_MSG_NOTE       # ANMERKUNG: Änderungen von Gefroren/Getaut wurden auf Ausgangs-Ansichtsfenster angewendet. Einige Änderungen sind möglicherweise im aktuellen Ansichtsfenster nicht sichtbar.
LAYERP_MSG_FAIL       # * Kein Status "Vorherige Layer" *

//======================================

//------ SHADEMODE
SHADEMODE_CMD              # SHADEMODE
SHADEMODE_MSG_MODE         # Aktueller Modus:
SHADEMODE_MSG_2D           # 2d Drahtkörper
SHADEMODE_MSG_3D           # 3d Drahtkörper
SHADEMODE_MSG_HIDDEN       # Verdeckt
SHADEMODE_MSG_FLAT         # Flach
SHADEMODE_MSG_GOURAUD      # Gouraud
SHADEMODE_MSG_FLAT+        # Flach+Kanten
SHADEMODE_MSG_GOURAUD+     # Gouraud+Kanten
SHADEMODE_MSG_REQUEST_MODE # Option eingeben [2d drahtkörper/3d drahtkörper/Verdeckt/Flach/Gouraud/fLach+kanten/gOuraud+kanten]
SHADEMODE_KWD_REQUEST_MODE # 2d 3d Verdeckt Flach Gouraud fLach+kanten gOuraud+kanten

//------ IMAGE
IMAGE_CMD                  # BILD
IMAGE_CMD_DLG_TITLE        # Bild-Manager
IMAGE_CMD_DLG_BTN_OK       # OK
IMAGE_CMD_DLG_BTN_CANCEL   # Abbrechen
IMAGE_CMD_DLG_BTN_HELP     # Hilfe
IMAGE_CMD_DLG_BTN_ATTACH   # Zuordnen
IMAGE_CMD_DLG_BTN_DETACH   # Lösen
IMAGE_CMD_DLG_BTN_UNLOAD   # Entfernen
IMAGE_CMD_DLG_BTN_RELOAD   # Neu laden
IMAGE_CMD_DLG_BTN_BROWSE   # Durchsuchen
IMAGE_CMD_DLG_BTN_SAVEPATH # Pfad speichern
IMAGE_CMD_DLG_BTN_DETAILS+ # Details >>
IMAGE_CMD_DLG_BTN_DETAILS- # Details <<
IMAGE_CMD_DLG_COL_NAME     # Bildname
IMAGE_CMD_DLG_COL_COUNT    # Anzahl
IMAGE_CMD_DLG_COL_STATUS   # Status
IMAGE_CMD_DLG_COL_SIZE     # Größe
IMAGE_CMD_DLG_COL_TYPE     # Typ
IMAGE_CMD_DLG_COL_DATE     # Datum
IMAGE_CMD_DLG_COL_PATH     # Gespeicherter Pfad
IMAGE_CMD_DLG_COL_FOUND    # Bild gefunden in
IMAGE_CMD_DLG_FRAME_PATH   # Bild gefunden in
IMAGE_CMD_DLG_IS_LOADED    # Geladen
IMAGE_CMD_DLG_IS_UNLOAD    # Entfernt
IMAGE_CMD_DLG_IS_RELOAD    # Neu geladen
IMAGE_CMD_DLG_IS_NOTFOUND  # Nicht gefunden
IMAGE_CMD_DLG_ERR_NOFILE1  # Bilddatei nicht gefunden !
IMAGE_CMD_DLG_ERR_NOFILE2  # Möchten Sie diesen Pfad wirklich verwenden ?

//------ IMAGE CMDLINE
IMAGE_CMD_CMDLINE          # -BILD
CMD_-IMAGE_MSG_OPTION      # Bildoption eingeben [?/Lösen/Pfad/Neuladen/Entfernen/Zuordnen] <Zuordnen>:
CMD_-IMAGE_KWD_OPTION      # ? Lösen Pfad Neuladen Entfernen Zuordnen
CMD_-IMAGE_?_LISTMASK      # Aufzulistende Bilder <*>:
CMD_-IMAGE_?_NO_IMAGES     # Keine Bilder in aktueller Zeichnung.
CMD_-IMAGE_?_NOSUCHIMAGES  # Keine entsprechenden Bildnamen gefunden.
CMD_-IMAGE_?_HEADER1       # Bildname                         Elemente   Pfad
CMD_-IMAGE_?_HEADER2       # ----------------                 ---------  ----------
CMD_-IMAGE_?_NR_IMAGEDEFS  # Gesamte Bilddefinitionen:
CMD_-IMAGE_?_NR_IMAGES     # Gesamte Bildelemente:
CMD_-IMAGE_P_LISTMASK      # Liste der Bilder für Pfadänderung eingeben:
CMD_-IMAGE_P_MSGNAME       # Bildname:
CMD_-IMAGE_P_OLDPATH       # Alter Pfad:
CMD_-IMAGE_P_NEWPATH       # Neuen Pfad eingeben:
CMD_-IMAGE_P_CHANGED       # Pfad wurde geändert.
CMD_-IMAGE_P_NOTCHANGED    # Pfad unverändert.
CMD_-IMAGE_P_FILENOTFOUND  # Hinweis: die Datei ist nicht vorhanden !
CMD_-IMAGE_D_LISTMASK      # Liste der zu lösenden Bilder eingeben:
CMD_-IMAGE_R_LISTMASK      # Liste der neu zu ladenden Bilder eingeben:
CMD_-IMAGE_R_RELOADING     #   Lädt neu...
CMD_-IMAGE_R_RELOADMSG     # Bild neu laden
CMD_-IMAGE_R_RELOADED      # geladen und neu verknüpft.
CMD_-IMAGE_U_LISTMASK      # Liste der zu entfernenden Bilder eingeben:
CMD_-IMAGE_A_FILENAME      # Namen der zuzuordnenden Bilddatei eingeben
CMD_-IMAGE_A_IMGNOTFOUND   # Bild nicht gefunden.

//------ IMAGECLIP
IMAGECLIP_CMD              # BILDZUSCHNEIDEN
SELECT_IMAGE_TO_CLIP       # Zuzuschneidendes Bild:
OPTION_IMAGE_TO_CLIP       # Option zum Zuschneiden des Bildes eingeben [Ein/Aus/Löschen/Neue Umgrenzung] <Neue>:
OPTION_IMAGE_TO_CLIP_KEYS  # Ein Aus Löschen Neue
OPTION_IMAGE_TO_CLIP_OLD   # Alte Umgrenzung löschen? [Nein/Ja] <Ja>:
KEYWRD_IMAGE_TO_CLIP_OLDK  # Ja Nein
OPTION_IMAGE_TO_CLIP_MODE  # Schnittflächentyp eingeben [Polygonal/Rechteckig] <Rechteckig>:
KEYWRD_IMAGE_TO_CLIP_MODEK # Rechteckig Polygonal
RECTANGLE_CLIP_POINT_1     # Ersten Eckpunkt angeben:
RECTANGLE_CLIP_POINT_2     # Gegenüberliegenden Eckpunkt definieren:
RECTANGLE_CLIP_POINT_FIRST # Ersten Punkt angeben:
RECTANGLE_CLIP_POINT_NEXT  # Nächsten Punkt angeben oder [Schliessen]:
RECTANGLE_CLIP_KEY_CLOSE   # Schliessen

//------ IMAGETRANSPARENCY
IMAGETRANSPARENCY_CMD      # TRANSPARENZ
SELECT_IMAGE_TO_TRANSP     # Bild(er) wählen :
SELECT_TRANSPARENCY_MODE   # Transparenzmodus eingeben [Ein/Aus]
KEYWRD_TRANSPARENCY_ON     # Ein
KEYWRD_TRANSPARENCY_OFF    # Aus

//------ IMAGEQUALITY
IMAGEQUALITY_CMD           # BILDQUALITÄT
SELECT_IMGQUALITY_MODE     # Einstellung für Bildqualität eingeben [Hoch/Entwurf]
SELECT_IMGQUALITY_KEYHIGH  # Hoch
SELECT_IMGQUALITY_KEYDRAFT # Entwurf

//------ IMAGEADJUST
IMAGEADJUST_CMD            # BILDANPASSEN
IMAGEADJUST_CMD_CMDLINE    # -BILDANPASSEN
SELECT_IMAGE_TO_ADJUST     # Bild wählen :
DLG_IMGADJ_TITLE           # Bild anpassen
DLG_IMGADJ_BRIGHT_LABEL    # Helligkeit
DLG_IMGADJ_BRIGHT_DARK     # Dunkel
DLG_IMGADJ_BRIGHT_LIGHT    # Hell
DLG_IMGADJ_CONTRAST_LABEL  # Kontrast
DLG_IMGADJ_CONTRAST_LOW    # Niedrig
DLG_IMGADJ_CONTRAST_HIGH   # Hoch
DLG_IMGADJ_FADE_LABEL      # Fade
DLG_IMGADJ_FADE_MIN        # Min
DLG_IMGADJ_FADE_MAX        # Max
DLG_IMGADJ_RESET           # Zurücksetzen
DLG_IMGADJ_UNDO            # Undo
DLG_IMGADJ_OK              # OK
DLG_IMGADJ_CANCEL          # Abbrechen
CMD_IMGADJ_SELECT_OPTION   # Bildoption eingeben [Kontrast/Fade/Helligkeit] <Helligkeit>:
CMD_IMGADJ_KEYWRDS         # Kontrast Fade Helligkeit
CMD_IMGADJ_OPTION_BRIGHT   # Wert für Helligkeit eingeben (0-100)
CMD_IMGADJ_OPTION_CONTRAST # Wert für Kontrast eingeben (0-100)
CMD_IMGADJ_OPTION_FADE     # Wert für Fade eingeben (0-100)
CMD_IMGADJ_ERROR_VALUE     # Wert muß eine Ganzzahl zwischen 0 und 100 sein

//------ IMAGEATTACH
IMAGEATTACH_CMD            # BILDZUORDNEN
DLG_IMGINS_TITLE           # Bild
DLG_IMGINS_MSG_NAME        # Name
DLG_IMGINS_BTN_FILE        # Durchsuchen...
DLG_IMGINS_CHK_PATH        # Pfad beibehalten
DLG_IMGINS_MSG_PATH        # Pfad
DLG_IMGINS_FRM_POINT       # Einfügepunkt
DLG_IMGINS_CHK_POINT       # am Bildschirm zeigen
DLG_IMGINS_MSG_X           # X:
DLG_IMGINS_MSG_Y           # Y:
DLG_IMGINS_MSG_Z           # Z:
DLG_IMGINS_FRM_SCALE       # Skalierung
DLG_IMGINS_CHK_SCALE       # am Bildschirm zeigen
DLG_IMGINS_MSG_SCALE       # Faktor
DLG_IMGINS_FRM_ROT         # Drehung
DLG_IMGINS_CHK_ROT         # am Bildschirm zeigen
DLG_IMGINS_MSG_ROT         # Winkel
DLG_IMGINS_BTN_OK          # OK
DLG_IMGINS_BTN_CANCEL      # Abbruch
DLG_IMGINS_SELECTTITLE     # Bilddatei auswählen
DLG_IMGINS_SELECTFILTER    # Alle Bilddateien|*.bmp;*.rle;*.dib;*.rst;*.gp4;*.mil;*.cal;*.cg4;*.flc;*.fli;*.bil;*.ig4;*.igs;*.jpg;*.pcx;*.pct;*.png;*.rlc;*.tga;*.tif;*.tiff|Alle Dateien (*.*)|*.*|BMP (*.bmp, *.rle, *.dib)|*.bmp;*.rle;*.dib|CALS1 (*.rst, *.gp4, *.mil, *.cal, *.cg4)|*.rst;*.gp4;*.mil;*.cal;*.cg4|FLIC (*.flc, *.fli)|*.flc;*.fli|GEOSPOT (*.bil)|*.bil|IG4 (*.ig4)|*.ig4|IGS (*.igs)|*.igs|JPEG (*.jpg)|*.jpg|PCX (*.pcx)|*.pcx|PICT (*.pct)|*.pct|PNG (*.png)|*.png|RLC (*.rlc)|*.rlc|TGA (*.tga)|*.tga|TIFF (*.tif, *.tiff)|*.tif;*.tiff||
DLG_IMGINS_INSERTPOINT     # Einfügepunkt angeben :
DLG_IMGINS_INSERTSCALE     # Skalierfaktor angeben :
DLG_IMGINS_INSERTROTATION  # Legen Sie den Drehwinkel fest :

//----- 3DMESH
3DMESH_CMD            # 3DNETZ
3DMESH_M_POINTS       # Größe des Netzes in Richtung M (Zeilen)  eingeben
3DMESH_N_POINTS       # Größe des Netzes in Richtung N (Spalten) eingeben
3DMESH_X_POINT_IN     # Position des Kontrollpunkts eingeben

//----- PFACE
PFACE_CMD             # PNETZ
PFACE_MSG_LOCATION1   # Position des Kontrollpunkts %d eingeben
PFACE_MSG_LOCATION2   # Position des Kontrollpunkts %d eingeben oder <Flächen definieren>
PFACE_MSG_FACEVERTEX  # Fläche %d, Kontrollpunkt %d
PFACE_MSG_VTXCOLLAY     # Kontrollpunktnummer eingeben oder [Farbe/Layer]
PFACE_MSG_VTXCOLLAYNEXT # Kontrollpunktnummer eingeben oder [Farbe/Layer] <nächste Fläche>
PFACE_KWD_COLLAY      # Color Layer
PFACE_KWD_COLLAY_LOC  # Farbe Layer
PFACE_ERR_INVAL_VTX   # Ungültiger Kontrollpunkt.
PFACE_ERR_INVAL_INT   # Wert muß eine Ganzzahl zwischen -5 und 5 sein, oder Optionstitel.

//----- TRACE
TRACE_CMD             # BAND
TRACE_MSG_WIDTH       # Bandbreite angeben
TRACE_MSG_START       # Startpunkt angeben
TRACE_MSG_NEXT        # Nächsten Punkt angeben

//----- 3DFACE
3DFACE_CMD            # 3DFLÄCHE
3DFACE_MSG_PT1        # Ersten Punkt angeben
3DFACE_MSG_PT2        # Zweiten Punkt angeben
3DFACE_MSG_PT3        # Dritten Punkt angeben
3DFACE_MSG_PT4        # Vierten Punkt angeben
3DFACE_MSG_INVIS      # oder [Unsichtbar]
3DFACE_KWD_INVIS      # Invisible
3DFACE_KWD_INVIS_LOC  # Unsichtbar
3DFACE_MSG_EXIT       # <beenden>
3DFACE_MSG_3SIDE      # <dreiseitige Fläche erstellen>

//----- EDGE
EDGE_CMD              # EDGE
EDGE_KWD_DISPLAY      # Display
EDGE_KWD_DISPLAY_LOC  # Anzeigen
EDGE_MSG_START        # Kante der 3D-Fläche zum Ein- und Ausschalten der Sichtbarkeit angeben oder [Anzeigen]
EDGE_MSG_SELECT       # Auswahlmethode für Anzeige verdeckter Kanten eingeben [AUswählen/ALles] <ALles>
EDGE_KWD_ALL          # All
EDGE_KWD_ALL_LOC      # ALles
EDGE_KWD_SELECT       # Select
EDGE_KWD_SELECT_LOC   # AUswählen
EDGE_MSG_REGEN        # * Regeneriert 3DFLÄCHE Objekte ...
EDGE_MSG_DONE         # abgeschlossen.
EDGE_MSG_NOEDGE       # Keine 3DFLÄCHE-Kanten gefunden.
EDGE_MSG_NOFACES      # Keine 3DFLÄCHE Objekte gefunden.

//----- TABSURF
TABSURF_CMD           # TABOB
TABSURF_SURFTAB1      # Aktuelle Drahtmodelldichte
TABSURF_MSG_PATH      # Objekt für Grundlinie wählen
TABSURF_MSG_VECTOR    # Objekt für Richtungsvektor wählen
TABSURF_ERR_PATH      # Grundlinienobjekt kann nicht zur Definition einer tabellarischen Fläche verwendet werden.
TABSURF_ERR_VECTOR    # Kein Objekt für Richtungsvektor.

//----- REVSURF
REVSURF_CMD           # ROTOB
REVSURF_SURFTAB12     # Aktuelle Drahtmodelldichte
REVSURF_MSG_PATH      # Zu rotierendes Objekt wählen
REVSURF_MSG_VECTOR    # Objekt wählen, das Rotationsachse definiert
REVSURF_MSG_STARTANG  # Startwinkel angeben
REVSURF_MSG_INCLANG   # Eingeschlossenen Winkel angeben (+=guz, -=uz)
REVSURF_ERR_PATH      # Kein Objekt zum Definieren einer Rotationsfläche.
REVSURF_ERR_VECTOR    # Kein Objekt für Rotationsachse.

//----- RULESURF
RULESURF_CMD          # REGELOB
RULESURF_SURFTAB1     # Aktuelle Drahtmodelldichte
RULESURF_MSG_PATH1    # Erste Definitionslinie wählen
RULESURF_MSG_PATH2    # Zweite Definitionslinie wählen
RULESURF_ERR_PATH     # Kein Objekt zum Definieren einer Regelfläche.
RULESURF_ERR_2POINTS  # Zwei Punkte definieren keine Fläche.
RULESURF_ERR_MIXED    # Offene und geschlossene Kurven können nicht kombiniert werden.

//----- EDGESURF
EDGESURF_CMD          # KANTOB
EDGESURF_SURFTAB12    # Aktuelle Drahtmodelldichte
EDGESURF_MSG_PATH     # Objekt %d für Kante wählen
EDGESURF_ERR_PATH     # Kein Objekt zum Definieren einer Fläche.
EDGESURF_ERR_ISPICK   # Objekt wurde bereits gewählt.
EDGESURF_ERR_GAP      # Kante %d berührt keine andere Kante.

//----- VPCLIP
VPCLIP_CMD                   # AFZUSCHNEIDEN
VPCLIP_MSG_SEL_VPORT         # Zuzuschneidendes Ansichtsfenster wählen
VPCLIP_ERR_SEL_VPORT         # Ausgewähltes Objekt war kein Ansichtsfenster.
VPCLIP_ERR_SEL_CLIPENT       # Ungültiges Ansichtsfenster-Schnittobjekt.
VPCLIP_ERR_SEL_NOTCLOSED     # Schnittobjekt muß geschlossen sein.
VPCLIP_KWD_SEL_CLIP_POLY     # Polygonal
VPCLIP_KWD_SEL_CLIP_POLY_LOC # Polygonal
VPCLIP_KWD_SEL_CLIP_DEL      # Delete
VPCLIP_KWD_SEL_CLIP_DEL_LOC  # Löschen
VPCLIP_MSG_SEL_CLIP0         # Objekt zum Zuschneiden wählen oder [Polygonal] <Polygonal>
VPCLIP_MSG_SEL_CLIP1         # Objekt zum Zuschneiden wählen oder [Polygonal/Löschen] <Polygonal>

//----- ARX
ARX_CMD             # ARX
ARX_MSG_START       # Geben Sie eine Option ein [?/Laden/Entfernen/Befehle]
ARX_KWD_START       # ? Laden Entfernen Befehle
ARX_MSG_COMMANDS    # Durch Erweiterungsprogramme registrierte Befehle
ARX_MSG_CMDGROUP    # Befehlsgruppe
ARX_MSG_CMDNEXT     # Drücken Sie die EINGABETASTE, um fortzufahren
ARX_MSG_LOADED      # Geladene Runtime Extension-Programme
ARX_MSG_LOADEDEND   # Ende der Liste.
ARX_MSG_UNLOAD      # Name der zu entladenden ARX/DBX/DLL Anwendung
ARX_MSG_UNLOAD_YES  # %s erfolgreich entfernt.
ARX_MSG_UNLOAD_NO   # Anwendung %s ist gelocked und kann nicht entfernt werden.
ARX_MSG_DLGTITLE    # ARX/DBX/DLL Datei auswählen
ARX_MSG_LOADFILE    # Name der zu ladenden ARX/DBX/DLL Anwendung
ARX_ERR_NOFILEFOUND # Kann die angegebene Datei nicht finden
ARX_MSG_LOADED_YES  # % erfolgreich geladen.
ARX_MSG_LOADED_NO   # %s nicht geladen - inkompatible Version oder fehlende Abhängigkeiten.
ARX_MSG_ARXLOCKED   # Anwendung '%s' ist gesperrt und kann nicht entfernt werden.
ARX_MSG_NOTLOADED   # Kann Anwendung '%s' nicht entfernen, da sie nicht geladen ist.

//----- MINSERT
MINSERT_CMD             # MEINFÜG
MINSERT_MSG_START       # Blocknamen eingeben oder [?]
MINSERT_KWD_START       # ?
MINSERT_ERR_BLOCKNAME   # Block ist nicht definiert. Keine Zeichnungsdatei für angegebenen Block gefunden.
MINSERT_ERR_INVALID     # Ungültiger Blockname.
MINSERT_ERR_NOEXPLODE   # ** Fehler: Blöcke künnen nicht mit dem Befehl MEINFÜG aufgelüst werden.
MINSERT_MSG_LISTBLOCKS  # Aufzulistende(n) Block/Blöcke eingeben <*>
MINSERT_MSG_DEFBLOCKS1  # Definierte Blöcke
MINSERT_MSG_DEFBLOCKS2  # Benutzer-   Unbenannte
MINSERT_MSG_DEFBLOCKS3  # Blöcke      Blöcke
MINSERT_MSG_NUMBERROWS  # Anzahl Zeilen eingeben (---) <1>
MINSERT_MSG_NUMBERCOLS  # Anzahl Spalten eingeben (|||) <1>
MINSERT_MSG_ROWCELLDIST # Abstand zwischen Zeilen eingeben oder Zelle angeben (---)
MINSERT_MSG_COLCELLDIST # Abstand zwischen Spalten angeben (|||)
MINSERT_MSG_SETATTRIBS  # Attributwerte eingeben

//----- XCLIP
XCLIP_CMD             # XZUSCHNEIDEN
XCLIP_MSG_START0      # Option für Ausschneiden eingeben
XCLIP_MSG_START1      # [Ein/Aus/Schnittiefe/Löschen/Polylinie generieren/Neue umgrenzung] <Neue>
XCLIP_KWD_START       # Ein Aus Schnittiefe Löschen Polylinie Neue
XCLIP_MSG_DELETEOLD   # Alte Umgrenzung(en) löschen? [Ja/Nein] <Ja>:
XCLIP_MSG_CLIP0       # Auswahl Zuschneide-Umgrenzung
XCLIP_MSG_CLIP1       # [Polylinie wählen/polyGonal/Rechteckig] <Rechteckig>
XCLIP_MSG_CLIP2       # Erste Ecke angeben
XCLIP_MSG_CLIP3       # Entgegengesetzte Ecke angeben
XCLIP_MSG_CLIP4       # Polylinie wählen
XCLIP_MSG_CLIP5       # Ersten Punkt angeben
XCLIP_MSG_CLIP6       # Nächsten Punkt angeben oder [Zurück]
XCLIP_KWD_CLIPNEW     # Polylinie polyGonal Rechteckig
XCLIP_KWD_YESNO       # Ja Nein
XCLIP_KWD_UNDO        # Zurück
XCLIP_MSG_CLIPFRONT   # Geben Sie vorderen Schnittpunkt an oder [Abstand/Entfernen]
XCLIP_MSG_CLIPBACK    # Geben Sie vorderen Schnittpunkt an oder [Abstand/Entfernen]
XCLIP_MSG_CLIPDIST    # Geben Sie den Abstand von der Umgrenzung an
XCLIP_KWD_CLIPDEPTH   # Abstand Entfernen
XCLIP_ERR_NOBLOCKXREF # Keine gefunden.
XCLIP_ERR_INELIGIBLE  # Element(e) ungeeignet für Zuschneiden
XCLIP_ERR_NOBOUNDARY  # Keine Zuschneide-Umgrenzung gefunden.
XCLIP_ERR_NEEDBOUND   # Block/XRef muá vor Angabe der Schnittiefe eine Zuschneide-Umgrenzung enthalten.
XCLIP_ERR_CLIPPLANE   # Fehler: Vordere Zuschneideebene hinter hinterer Zuschneideebene. Schnittiefe nicht angewendet.
XCLIP_ERR_SELFISECT1  # Ungültiger Punkt. Polygonsegmente dürfen sich nicht schneiden.
XCLIP_ERR_SELFISECT2  # Sich selbst schneidende Zuschneide-Umgrenzungen sind nicht zulässig.
XCLIP_ERR_NOPOLYLINE  # Gewähltes Objekt ist keine Polylinie.

//----- 3DARRAY
3DARRAY_CMD           # 3DREIHE
3DARRAY_MSG_MODE      # Anordnungstyp eingeben [Rechteckig/Polar] <R>
3DARRAY_KEY_MODE      # Rechteckig Polar
3DARRAY_MSG_NRROWS    # Zeilenanzahl eingeben (---) <1>
3DARRAY_MSG_NRCOLUMNS # Spaltenanzahl eingeben (|||) <1>
3DARRAY_MSG_NRLEVELS  # Ebenenanzahl eingeben (...) <1>
3DARRAY_MSG_DISTROW   # Zeilenabstand eingeben (---)
3DARRAY_MSG_DISTCOL   # Spaltenabstand eingeben (|||)
3DARRAY_MSG_DISTLEV   # Ebenenabstand eingeben (...)
3DARRAY_MSG_NRITEMS   # Anzahl der Elemente in der Anordnung angeben
3DARRAY_MSG_ANGLEFILL # Auszufüllenden Winkel angeben (+=ccw, -=cw) <360>
3DARRAY_MSG_DOROTATE  # Angeordnete Objekte drehen? [Ja/Nein] <J>
3DARRAY_KEY_YESNO     # Ja Nein
3DARRAY_MSG_AXISP1    # Mittelpunkt der Anordnung angeben
3DARRAY_MSG_AXISP2    # Zweiten Punkt auf Drehachse angeben
3DARRAY_ERR_NOTHING   # Anordnung mit einem Element, nichts zu verarbeiten.
3DARRAY_ERR_MOREITEMS # Elementanzahl muß höher sein als 1

//----- ALIGN
ALIGN_CMD             # AUSRICHTEN
ALIGN_MSG_SOURCE1     # Ersten Ursprungspunkt angeben
ALIGN_MSG_TARGET1     # Ersten Zielpunkt angeben
ALIGN_MSG_SOURCE2     # Zweiten Ursprungspunkt angeben
ALIGN_MSG_TARGET2     # Zweiten Zielpunkt angeben
ALIGN_MSG_SOURCE3     # Dritten Ursprungspunkt angeben oder <weiter>
ALIGN_MSG_TARGET3     # Dritten Zielpunkt angeben
ALIGN_MSG_DOSCALING   # Objekte anhand von Ausrichtepunkten skalieren?  [Ja/Nein] <N>
ALIGN_KEY_DOSCALING   # Ja Nein
ALIGN_ERR_COINC_S12   # Erster und zweiter Ursprungspunkt fallen zusammen.
ALIGN_ERR_COINC_S23   # Zweiter und dritter Ursprungspunkt fallen zusammen.
ALIGN_ERR_COINC_S13   # Erster und dritter Ursprungspunkt fallen zusammen.
ALIGN_ERR_COINC_T12   # Erster und zweiter Zielpunkt fallen zusammen.
ALIGN_ERR_COINC_T23   # Zweiter und dritter Zielpunkt fallen zusammen.
ALIGN_ERR_COINC_T13   # Erster und dritter Zielpunkt fallen zusammen.

//----- ROTATE3D
ROTATE3D_CMD             # 3DDREHEN
ROTATE3D_MSG_CURMODE     # Aktueller positiver Winkel
ROTATE3D_MSG_CLOCKWISE   # Im Uhrzeigersinn
ROTATE3D_MSG_CCLOCKWISE  # Gegen den Uhrzeigersinn
ROTATE3D_MSG_MAINOPTION1 # Ersten Punkt auf Achse angeben oder Achse definieren nach
ROTATE3D_MSG_MAINOPTION2 # [Objekt/Letztes/ANsicht/X-achse/Y-achse/Z-achse/2Punkte] <2Punkte>
ROTATE3D_KEY_MAINOPTION  # Objekt Letztes ANsicht X-achse Y-achse Z-achse 2Punkte
ROTATE3D_MSG_POINT1      # Ersten Achsenpunkt angeben
ROTATE3D_MSG_POINT2      # Zweiten Achsenpunkt angeben
ROTATE3D_MSG_ROTATION    # Drehwinkel angeben oder [Bezug]
ROTATE3D_KEY_ROTATION    # Bezug
ROTATE3D_MSG_VIEW        # Punkt auf Ansichtsrichtungsachse angeben <0,0,0>
ROTATE3D_MSG_XAXIS       # Punkt angeben auf der X -Achse <0,0,0>
ROTATE3D_MSG_YAXIS       # Punkt angeben auf der Y -Achse <0,0,0>
ROTATE3D_MSG_ZAXIS       # Punkt angeben auf der Z -Achse <0,0,0>
ROTATE3D_MSG_ANGREF      # Bezugswinkel angeben
ROTATE3D_MSG_ANGNEW      # Neuen Winkel angeben
ROTATE3D_MSG_SELOBJECT   # Linie, Kreis, Bogen oder 2D-Polyliniensegment auswählen
ROTATE3D_ERR_NOLASTAXIS  # Bisher wurde keine Achse verwendet.
ROTATE3D_ERR_NOOBJECT    # Kein Objekt ausgewählt.
ROTATE3D_ERR_WRONGOBJECT # Unzutreffender Objekttyp ausgewählt.

//----- MIRROR3D
MIRROR3D_CMD             # MIRROR3D
MIRROR3D_MSG_MAINOPTION1 # Ersten Punkt auf Spiegelebene (3 Punkte) angeben oder
MIRROR3D_MSG_MAINOPTION2 # [Objekt/Letztes/Z-achse/Ansicht/XY/YZ/ZX/3Punkte] <3Punkte>
MIRROR3D_KEY_MAINOPTION  # Objekt Letztes Z-achse Ansicht XY YZ ZX 3Punkte
MIRROR3D_MSG_POINT1      # Ersten Punkt auf Spiegelebene angeben
MIRROR3D_MSG_POINT2      # Zweiten Punkt auf Spiegelebene angeben
MIRROR3D_MSG_POINT3      # Dritten Punkt auf Spiegelebene angeben
MIRROR3D_MSG_POINTPLANE  # Punkt auf Spiegelebene angeben
MIRROR3D_MSG_POINTZAXIS  # Punkt auf der Z-Achse (Normale) der Spiegelebene angeben
MIRROR3D_MSG_VIEW        # Punkt auf der Ansichtsebene angeben <0,0,0>
MIRROR3D_MSG_XYPLANE     # Punkt angeben auf der XY -Ebene <0,0,0>
MIRROR3D_MSG_YZPLANE     # Punkt angeben auf der YZ -Ebene <0,0,0>
MIRROR3D_MSG_ZXPLANE     # Punkt angeben auf der ZX -Ebene <0,0,0>
MIRROR3D_MSG_SELOBJECT   # Kreis, Bogen oder 2D-Polyliniensegment auswählen
MIRROR3D_MSG_DELOBJECT   # Quellobjekte löschen? [Ja/Nein] <N>
MIRROR3D_KEY_DELOBJECT   # Ja Nein
MIRROR3D_ERR_NOLASTPLANE # Bisher wurde keine Ebene verwendet.
MIRROR3D_ERR_NOOBJECT    # Kein Objekt ausgewählt.
MIRROR3D_ERR_WRONGOBJECT # Unzutreffender Objekttyp ausgewählt.

//----- SKETCH
SKETCH_CMD            # SKIZZE
SKETCH_MSG_MODE       # Aktuelle Skizzier-Elemente
SKETCH_MSG_INCREMENT  # Skizziergenauigkeit
SKETCH_MSG_MAIN       # Skizze:  Stift eXit Beenden sPeichern Löschen Verbinden .
SKETCH_KEY_PEN        # Stift
SKETCH_KEY_EXIT       # eXit
SKETCH_KEY_QUIT       # Beenden
SKETCH_KEY_RECORD     # sPeichern
SKETCH_KEY_ERASE      # Löschen
SKETCH_KEY_CONNECT    # Verbinden
SKETCH_KEY_CONTINUE   # .
SKETCH_MSG_PENDOWN    # <Stift unten>
SKETCH_MSG_PENUP      # <Stift oben>
SKETCH_MSG_DONELINES  # %d Linie(n) gespeichert.
SKETCH_MSG_DONEPOLYS  # %d Polylinie(n) mit %d Seiten gespeichert.
SKETCH_MSG_MOVETOLINE # Verbinden:  Zum Endpunkt der Linie gehen.
SKETCH_MSG_SELECTEND  # Löschen:  Das Ende der Löschfunktion wählen.
SKETCH_ERR_NOLASTPNT  # Es ist kein letzter Punkt bekannt.
SKETCH_ERR_CONNECT    # Verbinden abgebrochen.
SKETCH_ERR_ERASE      # Löschen abgebrochen.
SKETCH_ERR_RECORDED   # Nichts gespeichert.
SKETCH_ERR_SKETCHED   # Nichts gezeichnet.

//----- ATTREDEF
ATTREDEF_CMD            # ATTREDEF
ATTREDEF_CMD_ALT        # AT
ATTREDEF_MSG_BLOCKNAME  # Blocknamen zum Neudefinieren eingeben
ATTREDEF_ERR_BLOCKNAME  # Block ist nicht definiert. Versuchen Sie es erneut.
ATTREDEF_ERR_NOINSERTS  # Keine Blockeinfügungen %s zum Neudefinieren gefunden.
ATTREDEF_MSG_NEWBLOCK   # Objekte für neuen Block wählen...
ATTREDEF_ERR_NEWBLOCK   # Kein neuer Block ausgewählt. Versuchen Sie es erneut.
ATTREDEF_ERR_NOATTRIBS  # Neuer Block hat keine Attribute.
ATTREDEF_MSG_REFPOINT   # Basispunkt der Einfügung für neuen Block angeben

//----- Acis common errors
ACIS_ERR_UNKNOW_ERR   # * ACIS error : unbekannter Fehler
ACIS_ERR_INVALIDINP   # * ACIS error : fehlerhafte Eingabe
ACIS_ERR_OUTOFRANGE   # * ACIS error : Daten zu gross oder zu klein
ACIS_ERR_INVALIDAXIS  # * ACIS error : Achse(n) falsch
ACIS_ERR_MISSINGACIS0 # * ACIS System unvollständig
ACIS_ERR_MISSINGACIS1 # * Das ACIS System ist nicht vollständig installiert / verfügbar !
ACIS_ERR_MISSINGACIS2 # EMPFEHLUNG: Installieren Sie die AutoCAD-2004-Demo-Version (30-Tage-Test)
ACIS_ERR_MISSINGACIS3 # auf diesem Rechner. Es ist NICHT erforderlich, Dateien zu kopieren oder
ACIS_ERR_MISSINGACIS4 # Suchpfade zu setzen - LT-Extender aktiviert das ACIS System automatisch !

//----- BOX (Acis)
ACIS_BOX_CMD          # QUADER
ACIS_BOX_MSG_POINT1   # Ecke des Quaders angeben oder [MIttelpunkt] <0,0,0>
ACIS_BOX_KEY_POINT1   # MIttelpunkt
ACIS_BOX_MSG_CENTER   # Mittelpunkt des Quaders angeben <0,0,0>
ACIS_BOX_MSG_CORNER   # Ecke angeben oder [Würfel/Länge]
ACIS_BOX_KEY_CORNER   # Würfel Länge
ACIS_BOX_MSG_LENGTH   # Länge angeben
ACIS_BOX_MSG_WIDTH    # Breite angeben
ACIS_BOX_MSG_HEIGHT   # Höhe angeben

//----- WEDGE (Acis)
ACIS_WEDGE_CMD        # KEIL
ACIS_WEDGE_MSG_POINT1 # Ecke des Keils angeben oder [MIttelpunkt] <0,0,0>
ACIS_WEDGE_KEY_POINT1 # MIttelpunkt
ACIS_WEDGE_MSG_CENTER # Mittelpunkt des Keils angeben <0,0,0>
ACIS_WEDGE_MSG_CORNER # Entgegengesetzte Ecke angeben [Würfel/Länge]
ACIS_WEDGE_KEY_CORNER # Würfel Länge
ACIS_WEDGE_MSG_LENGTH # Länge angeben
ACIS_WEDGE_MSG_WIDTH  # Breite angeben
ACIS_WEDGE_MSG_HEIGHT # Höhe angeben

//----- SPHERE (Acis)
ACIS_SPHERE_CMD        # KUGEL
ACIS_SPHERE_MSG_CENTER # Mittelpunkt der Kugel angeben <0,0,0>
ACIS_SPHERE_MSG_RADIUS # Radius der Kugel angeben oder [Durchmesser]
ACIS_SPHERE_MSG_DIAM   # Durchmesser angeben
ACIS_SPHERE_KEY_DIAM   # Durchmesser

//----- CONE (Acis)
ACIS_CONE_CMD         # KEGEL
ACIS_CONE_MSG_CENTER  # Mittelpunkt für Basis des Kegels angeben oder [Elliptisch] <0,0,0>
ACIS_CONE_KEY_ELLIPSE # Elliptisch
ACIS_CONE_MSG_POINT1  # Achsenendpunkt der Ellipse für Basis des Kegels angeben oder [Mittelpunkt]
ACIS_CONE_MSG_POINT2  # Zweiten Achsenendpunkt der Ellipse für Basis des Kegels angeben
ACIS_CONE_KEY_CENTER  # Mittelpunkt
ACIS_CONE_MSG_POINTX1 # Zentrum der Ellipse für Basis des Kegels angeben <0,0,0>
ACIS_CONE_MSG_POINTX2 # Achsenendpunkt der Ellipse für Basis des Kegels angeben
ACIS_CONE_MSG_LENGTH  # Länge der zweiten Achse für Basis des Kegels angeben
ACIS_CONE_MSG_RADIUS  # Radius für Basis des Kegels angeben oder [Durchmesser]
ACIS_CONE_KEY_DIAM    # Durchmesser
ACIS_CONE_MSG_DIAM    # Durchmesser für Basis des Kegels angeben
ACIS_CONE_MSG_HEIGHT  # Kegelhöhe angeben oder [Scheitelpunkt]
ACIS_CONE_KEY_APEX    # Scheitelpunkt
ACIS_CONE_MSG_APEX    # Scheitelpunkt angeben

//----- CYLINDER (Acis)
ACIS_CYLINDER_CMD     # ZYLINDER
ACIS_CYL_MSG_CENTER   # Mittelpunkt für Basis des Zylinders angeben oder [Elliptisch] <0,0,0>
ACIS_CYL_KEY_ELLIPSE  # Elliptisch
ACIS_CYL_MSG_POINT1   # Achsenendpunkt der Ellipse für Basis des Zylinders angeben oder [Mittelpunkt]
ACIS_CYL_MSG_POINT2   # Zweiten Achsenendpunkt der Ellipse für Basis des Zylinders angeben
ACIS_CYL_KEY_CENTER   # Mittelpunkt
ACIS_CYL_MSG_POINTX1  # Zentrum der Ellipse für Basis des Zylinders angeben <0,0,0>
ACIS_CYL_MSG_POINTX2  # Achsenendpunkt der Ellipse für Basis des Zylinders angeben
ACIS_CYL_MSG_LENGTH   # Länge der zweiten Achse für Basis des Zylinders angeben
ACIS_CYL_MSG_RADIUS   # Radius für Basis des Zylinders angeben oder [Durchmesser]
ACIS_CYL_KEY_DIAM     # Durchmesser
ACIS_CYL_MSG_DIAM     # Durchmesser für Basis des Zylinders angeben
ACIS_CYL_MSG_HEIGHT   # Höhe des Zylinders angeben oder [Mittelpunkt vom anderen ende]
ACIS_CYL_KEY_APEX     # Mittelpunkt
ACIS_CYL_MSG_APEX     # Mittelpunkt des anderen Zylinderendes angeben

//----- TORUS (Acis)
ACIS_TORUS_CMD        # TORUS
ACIS_TORUS_MSG_CENTER # Mittelpunkt des Torus angeben <0,0,0>
ACIS_TORUS_MSG_RADMAX # Radius des Torus angeben oder [Durchmesser]
ACIS_TORUS_MSG_RADMIN # Radius des Rohrs angeben oder [Durchmesser]
ACIS_TORUS_MSG_DIAM   # Durchmesser angeben
ACIS_TORUS_KEY_DIAM   # Durchmesser

//----- EXTRUDE (Acis)
ACIS_EXTRUDE_CMD        # EXTRUSION
ACIS_EXTRUDE_MSG_HEIGHT # Extrusionshöhe angeben oder [Pfad]
ACIS_EXTRUDE_MSG_TAPER  # Verjüngungswinkel für Extrusion angeben <0>
ACIS_EXTRUDE_MSG_PATH   # Extrusionspfad wählen oder [verJüngung]
ACIS_EXTRUDE_MSG_PATH1  # Extrusionspfad wählen
ACIS_EXTRUDE_KEY_PATH   # Pfad
ACIS_EXTRUDE_KEY_TAPER  # verJüngung
ACIS_EXTRUDE_ERR_NOENT  # Nichts gewählt.
ACIS_EXTRUDE_ERR_FAIL   # Kann gewähltes Objekt nicht extrudieren.
ACIS_EXTRUDE_ERR_FAIL1  # Kann %d gewählte(s) Objekt(e) nicht extrudieren.
ACIS_EXTRUDE_ERR_PATH   # Kann entlang dieses Pfads nicht extrudieren.
ACIS_EXTRUDE_ERR_ANGLE  # Winkel muß größer als -90 Grad und kleiner als 90 Grad sein.

//----- REVOLVE (Acis)
ACIS_REVOLVE_CMD        # ROTATION
ACIS_REVOLVE_MSG_AXIS1  # Startpunkt der Rotationsachse angeben oder
ACIS_REVOLVE_MSG_AXIS2  # Achse durch [Objekt/X (Achse)/Y (Achse)/Z (Achse)] definieren
ACIS_REVOLVE_MSG_AXIS3  # Endpunkt der Achse angeben
ACIS_REVOLVE_KEY_AXIS   # Objekt X Y Z
ACIS_REVOLVE_MSG_ANGLE  # Rotationswinkel angeben <360>
ACIS_REVOLVE_MSG_ENTITY # Objekt wählen:
ACIS_REVOLVE_ERR_NOENT  # Nichts gewählt.
ACIS_REVOLVE_ERR_FAIL   # Kann gewähltes Objekt nicht rotieren.
ACIS_REVOLVE_ERR_FAIL1  # Kann %d gewählte(s) Objekt(e) nicht rotieren.
ACIS_REVOLVE_ERR_AXIS1  # Achse muß Linie oder Polylinie sein.
ACIS_REVOLVE_ERR_AXIS2  # Diese Polylinie definiert keine Achse.

//----- INTERFERE (Acis)
ACIS_INTERFERE_CMD         # ÜBERLAG
ACIS_INTERFERE_MSG_SET1    # Ersten Satz Volumenkörper wählen
ACIS_INTERFERE_MSG_SET2    # Zweiten Satz Volumenkörper wählen
ACIS_INTERFERE_MSG_COMP1   # * Vergleicht %d Volumenkörper miteinander.
ACIS_INTERFERE_MSG_COMP2   # * Vergleicht %d Volumenkörper mit %d Volumenkörper.
ACIS_INTERFERE_MSG_NOTINT  # Volumenkörper überlagern sich nicht.
ACIS_INTERFERE_MSG_INTER1  # * Sich überlagernde Volumenkörper (Satz 1) :
ACIS_INTERFERE_MSG_INTER2  # *                                 (Satz 2) :
ACIS_INTERFERE_MSG_INTER3  # * Sich überlagernde Paare                  :
ACIS_INTERFERE_MSG_INTER4  # * Sich überlagernde Volumenkörper :
ACIS_INTERFERE_MSG_INTER5  # * Sich überlagernde Paare         :
ACIS_INTERFERE_MSG_CREATE  # Sich überlagernde Volumenkörper erstellen? [Ja/Nein] <N>
ACIS_INTERFERE_MSG_SHOW    # Paare sich überlagernder Volumenkörper hervorheben? [Ja/Nein] <N>
ACIS_INTERFERE_KEY_YESNO   # Ja Nein
ACIS_INTERFERE_MSG_SHOWOPT # Option eingeben [Nächstes paar/eXit] <Nächstes>:
ACIS_INTERFERE_KEY_SHOWOPT # Nächstes eXit
ACIS_INTERFERE_ERR_NOENT   # Keine Volumenkörper gewählt.
ACIS_INTERFERE_ERR_SINGLE  # Nur einen Volumenkörper im ersten Satz gewählt.

//----- SOLIDEDIT (Acis)
ACIS_SOLIDEDIT_CMD              # VOLKÖRPERBEARB
ACIS_SOLEDIT_MSG_CHECKING       # Automatische Überprüfung der Bearbeitung von Volumenkörpern
ACIS_SOLEDIT_MSG_MAINOPTIONS    # Bearbeitungsoption für Volumenkörper eingeben [Fläche/Kante/Volumenkörper/Zurück/eXit] <eXit>
ACIS_SOLEDIT_KEY_MAINOPTIONS    # Fläche Kante Volumenkörper Zurück eXit
ACIS_SOLEDIT_MSG_FACEOPTIONS    # Bearbeitungsoption für Flächen eingeben [Extrusion/Schieben/Drehen/Versetzen/verJüngung/Löschen/Kopieren/Farbe/Zurück/eXit] <eXit>
ACIS_SOLEDIT_KEY_FACEOPTIONS    # Extrusion Schieben Drehen Versetzen verJüngung Löschen Kopieren Farbe Zurück eXit
ACIS_SOLEDIT_MSG_EDGEOPTIONS    # Bearbeitungsoption für Kanten eingeben [Kopieren/Farbe/Zurück/eXit] <eXit>
ACIS_SOLEDIT_KEY_EDGEOPTIONS    # Kopieren Farbe Zurück eXit
ACIS_SOLEDIT_MSG_BODYOPTIONS    # Bearbeitungsoption für Volumenkörper eingeben [Aufprägen/volumenkörper Trennen/Wandstärke/Bereinigen/Überprüfen/Zurück/eXit] <eXit>
ACIS_SOLEDIT_KEY_BODYOPTIONS    # Aufprägen volumenkörper Wandstärke Bereinigen Überprüfen Zurück eXit
ACIS_SOLEDIT_MSG_UNDOCOMPLETE   # Der Befehl wurde vollständig gelöscht.
ACIS_SOLEDIT_MSG_SELECTSOLID    # 3D-Volumenkörper wählen
ACIS_SOLEDIT_ERR_SELECTSOLID    # 3D-Volumenkörper muß gewählt werden.
ACIS_SOLEDIT_ERR_NOSOLIDDETECT  # Keine Volumenkörper gefunden.
ACIS_SOLEDIT_MSG_SELECTEDGES    # Kanten wählen oder [Zurück/Entfernen]
ACIS_SOLEDIT_KEY_SELECTEDGES    # Zurück Entfernen
ACIS_SOLEDIT_MSG_SELECTFACES    # Flächen wählen oder [Zurück/Entfernen]
ACIS_SOLEDIT_KEY_SELECTFACES    # ZUrück/Entfernen
ACIS_SOLEDIT_MSG_SELECTFACESALL # Flächen wählen oder [Zurück/Entfernen/ALLE]
ACIS_SOLEDIT_KEY_SELECTFACESALL # Zurück Entfernen ALLE
ACIS_SOLEDIT_MSG_IMPRINTSOURCE  # Aufzuprägendes Objekt wählen
ACIS_SOLEDIT_MSG_IMPRINTSOURCEN # Quellobjekt löschen [Ja/Nein] <N>
ACIS_SOLEDIT_MSG_IMPRINTSOURCEY # Quellobjekt löschen [Ja/Nein] <J>
ACIS_SOLEDIT_KEY_YESNO          # Ja Nein
ACIS_SOLEDIT_MSG_VALIDSOLID     # Dieses Objekt ist ein gültiges ShapeManager-Objekt.
ACIS_SOLEDIT_MSG_INVALIDSOLID   # Dieses Objekt ist *KEIN* gültiges ShapeManager-Objekt.
ACIS_SOLEDIT_ERR_SELECTEDGEORG  # Kante auf dem ursprünglich gewählten Volumenkörper wählen.
ACIS_SOLEDIT_ERR_SELECTFACEORG  # Fläche auf dem ursprünglich gewählten Volumenkörper wählen.
ACIS_SOLEDIT_ERR_NOTSEPARATABLE # Der ausgewählte Volumenkörper verfügt nicht über mehrere Teilvolumen.
ACIS_SOLEDIT_ERR_NOFACESTODO    # Keine Flächen gewählt.
ACIS_SOLEDIT_MSG_REMFACES       # Flächen entfernen oder [Zurück/Hinzufügen/ALLE]
ACIS_SOLEDIT_MSG_SELFACES       # Flächen wählen oder [Zurück/Entfernen/ALLE]
ACIS_SOLEDIT_MSG_FACEUNDODONE   # Die Flächenauswahl wurde vollständig rückgängig gemacht.
ACIS_SOLEDIT_MSG_NRFACESADDED   # %d Flächen gefunden, %d hinzugefügt.
ACIS_SOLEDIT_MSG_NRFACESREMOVED # %d Flächen gefunden, %d entfernt.
ACIS_SOLEDIT_ERR_NOFACESTODO    # Keine Kanten gewählt.
ACIS_SOLEDIT_MSG_REMEDGES       # Kanten entfernen oder [Zurück/Hinzufügen/ALLE]
ACIS_SOLEDIT_MSG_SELEDGES       # Kanten wählen oder [Zurück/Entfernen/ALLE]
ACIS_SOLEDIT_MSG_EDGEUNDODONE   # Die Kantenauswahl wurde vollständig rückgängig gemacht.
ACIS_SOLEDIT_MSG_NREDGESADDED   # %d Kanten gefunden, %d hinzugefügt.
ACIS_SOLEDIT_MSG_NREDGESREMOVED # %d Kanten gefunden, %d entfernt.
ACIS_SOLEDIT_ERR_NOEDGESTODO    # Keine Kanten gewählt.
ACIS_SOLEDIT_KEY_UNDOADDALL     # Zurück Hinzufügen ALLE
ACIS_SOLEDIT_KEY_UNDOREMALL     # Zurück Entfernen ALLE
ACIS_SOLEDIT_MSG_OFFSETDIST     # Abstand angeben
ACIS_SOLEDIT_MSG_FACESEXTHEIGHT # Extrusionshöhe angeben oder [Pfad]
ACIS_SOLEDIT_MSG_FACESEXTTAPER  # Verjüngungswinkel für Extrusion angeben <0>
ACIS_SOLEDIT_MSG_FACESEXTPATH   # Extrusionspfad wählen
ACIS_SOLEDIT_KEY_FACESEXTPATH   # Pfad
ACIS_SOLEDIT_ERR_ANGLE          # Winkel muß größer als -90 Grad und kleiner als 90 Grad sein.
ACIS_SOLEDIT_ERR_NOPATH         # Keine Auswahl für Pfad.
ACIS_SOLEDIT_MSG_FACESMOVE1     # Basispunkt der Verschiebung angeben
ACIS_SOLEDIT_MSG_FACESMOVE2     # Zweiten Punkt der Verschiebung angeben
ACIS_SOLEDIT_MSG_FACESROTATE    # Achsenpunkt angeben oder [Achse von objekt/ANsicht/X-achse/Y-achse/Z-achse] <2Punkte>
ACIS_SOLEDIT_MSG_FACESROTATEP1  # Ersten Punkt auf der Rotationsachse angeben
ACIS_SOLEDIT_MSG_FACESROTATEP2  # Zweiten Punkt auf der Rotationsachse angeben
ACIS_SOLEDIT_MSG_FACESROTATEPT  # Ursprung der Drehung angeben <0,0,0>
ACIS_SOLEDIT_MSG_FACESROTATEANG # Drehwinkel angeben oder [Bezug]
ACIS_SOLEDIT_KEY_FACESROTATE    # 2Punkte Achse ANsicht X Y Z
ACIS_SOLEDIT_KEY_FACESROTATEANG # Bezug
ACIS_SOLEDIT_KEY_FACESROTANG1   # Bezugswinkel (Startwinkel) angeben <0>
ACIS_SOLEDIT_KEY_FACESROTANG2   # Zweiten Punkt angeben
ACIS_SOLEDIT_KEY_FACESROTANG3   # Endwinkel angeben
ACIS_SOLEDIT_MSG_FACESROTATEOBJ # Für die Achse zu verwendende Kurve wählen
ACIS_SOLEDIT_ERR_FACESROTATEOBJ # Kann dieses Objekt nicht als Rotationsachse verwenden.
ACIS_SOLEDIT_MSG_FACESOFFSDIST  # Abstand angeben
ACIS_SOLEDIT_MSG_FACESTAPERPT1  # Basispunkt angeben
ACIS_SOLEDIT_MSG_FACESTAPERPT2  # Anderen Punkt auf der Verjüngungsachse angeben
ACIS_SOLEDIT_MSG_FACESTAPERANG  # Verjüngungswinkel angeben
ACIS_SOLEDIT_ERR_FACESTAPERANG  # Der Verjüngungswinkel darf nicht 0 betragen und muß zwischen -90 und 90 liegen.

//----- STLOUT (Acis)
ACIS_STLOUT_CMD           # STLOUT
ACIS_STLOUT_MSG_SELECT    # Wählen Sie einen einzelnen Volumenkörper für STL ..
ACIS_STLOUT_MSG_FILEMODE  # Eine binäre STL-Datei erstellen? [Ja/Nein] <J>
ACIS_STLOUT_KEY_FILEMODE  # Ja Nein
ACIS_STLOUT_MSG_FILETYPE  # Stereolithographie (*.stl)
ACIS_STLOUT_MSG_FILEDLG   # STL Datei erstellen
ACIS_STLOUT_ERR_NOSOLIDS  # Keine Volumenkörper gewählt.
ACIS_STLOUT_ERR_USESINGLE # Nur ein Volumenkörper pro Datei erlaubt.
ACIS_STLOUT_MSG_FILENAME  # STL-Dateinamen eingeben

//----- ACISIN (Acis)
ACIS_ACISIN_CMD           # ACISIN
ACIS_ACISIN_DLG_TITLE     # ACIS-Datei wählen
ACIS_ACISIN_FILENAME      # SAT-Dateinamen eingeben

//----- ACISOUT (Acis)
ACIS_ACISOUT_CMD          # ACISOUT
ACIS_ACISOUT_DLG_TITLE    # ACIS-Datei erstellen
ACIS_ACISOUT_NOTHING      # Nichts gewählt
ACIS_ACISOUT_NO_SOLIDS    # Keine Volumenkörper, Regionen oder ShapeManager-Objekte gewählt.
ACIS_ACISOUT_IGNORED      # Objekte ignoriert, weil sie keine Regionen, Volumenkörper oder ShapeManager-Objekte sind.
ACIS_ACISOUT_FILENAME     # SAT-Dateinamen eingeben

//----- SECTION (Acis)
ACIS_SECTION_CMD          # QUERSCHNITT
ACIS_SECTION_MSG_MAIN     # Ersten Punkt auf Schnittebene angeben durch [Objekt/ZAchse/Ansicht/XY/YZ/ZX/3Punkte] <3Punkte>
ACIS_SECTION_KEY_MAIN     # Objekt ZAchse Ansicht XY YZ ZX 3Punkte
ACIS_SECTION_MSG_POINT1   # Ersten Punkt auf Ebene angeben
ACIS_SECTION_MSG_POINT2   # Zweiten Punkt auf Ebene angeben
ACIS_SECTION_MSG_POINT3   # Dritten Punkt auf Ebene angeben
ACIS_SECTION_MSG_POINT_XY # Punkt auf XY-Ebene <0,0,0> angeben
ACIS_SECTION_MSG_POINT_YZ # Punkt auf YZ-Ebene <0,0,0> angeben
ACIS_SECTION_MSG_POINT_ZX # Punkt auf ZX-Ebene <0,0,0> angeben
ACIS_SECTION_MSG_POINT_V  # Punkt auf der aktuellen Ansichtsebene <0,0,0> angeben
ACIS_SECTION_MSG_POINT_Z1 # Punkt auf der Schnittebene angeben
ACIS_SECTION_MSG_POINT_Z2 # Punkt auf Z-Achse (Normale) zur Ebene angeben
ACIS_SECTION_MSG_OBJECT   # Kreis, Ellipse, Bogen, Spline oder Polylinie wählen
ACIS_SECTION_ERR_NOSOLIDS # Keine Volumenkörper gewählt.
ACIS_SECTION_ERR_NOPLANE  # Kann Ebene des gewählten Objekts nicht extrahieren.

//----- SLICE (Acis)
ACIS_SLICE_CMD            # KAPPEN
ACIS_SLICE_MSG_MAIN       # Ersten Punkt auf Schnittebene angeben durch [Objekt/ZAchse/Ansicht/XY/YZ/ZX/3Punkte] <3Punkte>
ACIS_SLICE_KEY_MAIN       # Objekt ZAchse Ansicht XY YZ ZX 3Punkte
ACIS_SLICE_MSG_POINT1     # Ersten Punkt auf Ebene angeben
ACIS_SLICE_MSG_POINT2     # Zweiten Punkt auf Ebene angeben
ACIS_SLICE_MSG_POINT3     # Dritten Punkt auf Ebene angeben
ACIS_SLICE_MSG_POINT_XY   # Punkt auf XY-Ebene <0,0,0> angeben
ACIS_SLICE_MSG_POINT_YZ   # Punkt auf YZ-Ebene <0,0,0> angeben
ACIS_SLICE_MSG_POINT_ZX   # Punkt auf ZX-Ebene <0,0,0> angeben
ACIS_SLICE_MSG_POINT_V    # Punkt auf der aktuellen Ansichtsebene <0,0,0> angeben
ACIS_SLICE_MSG_POINT_Z1   # Punkt auf der Schnittebene angeben
ACIS_SLICE_MSG_POINT_Z2   # Punkt auf Z-Achse (Normale) zur Ebene angeben
ACIS_SLICE_MSG_OBJECT     # Kreis, Ellipse, Bogen, Spline oder Polylinie wählen
ACIS_SLICE_MSG_SIDE       # Punkt auf der gewünschten Seite der Ebene angeben oder [Beide seiten behalten]
ACIS_SLICE_KEY_SIDE       # Beide
ACIS_SLICE_ERR_NOSOLIDS   # Keine Volumenkörper gewählt.
ACIS_SLICE_ERR_NOPLANE    # Kann Ebene des gewählten Objekts nicht extrahieren.

//----- UNION (Acis)
ACIS_UNION_CMD            # VEREINIG
ACIS_UNION_ERR_ENTITIES   # Zumindest 2 Volumenkörper oder koplanare Regionen müssen gewählt werden.
ACIS_UNION_ERR_ENTITIESX  # Zumindest 2 koplanare Regionen müssen gewählt werden.

//----- INTERSECT (Acis)
ACIS_INTERSECT_CMD           # SCHNITTMENGE
ACIS_INTERSECT_ERR_ENTITIES  # Zumindest 2 Volumenkörper oder koplanare Regionen müssen gewählt werden.
ACIS_INTERSECT_ERR_ENTITIESX # Zumindest 2 koplanare Regionen müssen gewählt werden.
ACIS_INTERSECT_ERR_SOLID     # Kein Volumenkörper erstellt - gelöscht.
ACIS_INTERSECT_ERR_REGION    # Keine Region erstellt - gelöscht.

//----- SUBTRACT (Acis)
ACIS_SUBTRACT_CMD           # DIFFERENZ
ACIS_SUBTRACT_ERR_ENTITIES  # Zumindest 2 Volumenkörper oder koplanare Regionen müssen gewählt werden.
ACIS_SUBTRACT_ERR_ENTITIESX # Zumindest 2 koplanare Regionen müssen gewählt werden.
ACIS_SUBTRACT_MSG_SELECT_A  # Volumenkörper und Regionen, von denen subtrahiert werden soll, wählen ...
ACIS_SUBTRACT_MSG_SELECT_AX # Regionen, von denen subtrahiert werden soll, wählen ...
ACIS_SUBTRACT_MSG_SELECT_B  # Volumenkörper und Regionen für Subtraktion wählen ...
ACIS_SUBTRACT_MSG_SELECT_BX # Regionen für Subtraktion wählen ...

//----- XOPEN
XOPEN_CMD
XOPEN_ERR_NOXREF      # Objekt ist kein XRef.
XOPEN_ERR_NOSELECTION # Kein XRef ausgewählt.

//----- AI_3D
AI_3D_CMD             # 3D
AI_3D_MSG1            # Option eingeben
AI_3D_MSG2            # [Quader/KEGel/Schale/KUPpel/Netz/Pyramide/KUGel/Torus/KEIl]
AI_3D_KEYS            # Quader KEGel Schale KUPpel Netz Pyramide KUGel Torus KEIl

//----- AI_BOX
AI_BOX_CMD            # AI_BOX
AI_BOX_MSG_CORNER     # Eckpunkt für Quader
AI_BOX_MSG_LENGTH     # Länge für Quader
AI_BOX_MSG_WIDTH      # Breite des Quaders angeben oder [Würfel]
AI_BOX_MSG_HEIGHT     # Höhe für Quader
AI_BOX_KEY_CUBE       # Würfel
AI_BOX_MSG_ROTATE     # Drehwinkel des Quaders um Z-Achse angeben oder [Referenz]

//----- AI_CONE
AI_CONE_CMD           # AI_CONE
AI_CONE_MSG_CENTER    # Mittelpunkt für Basis des Kegels angeben
AI_CONE_MSG_BASERAD   # Radius für Basis des Kegels angeben oder [Durchmesser] <0>
AI_CONE_MSG_BASEDIA   # Durchmesser für Basis des Kegels angeben <0>
AI_CONE_MSG_TOPRAD    # Radius für Spitze des Kegels angeben oder [Durchmesser] <0>
AI_CONE_MSG_TOPDIA    # Durchmesser für Spitze des Kegels angeben <0>
AI_CONE_MSG_HEIGHT    # Höhe des Kegels angeben
AI_CONE_MSG_SURF1     # Segmentanzahl für Oberfläche des Kegels angeben <16>
AI_CONE_KEY_DIAM      # Durchmesser
AI_CONE_ERR_2DIAM0    # Die Radien/Durchmesser für können nicht gleichzeitig 0 sein.

//----- AI_SPHERE
AI_SPHERE_CMD         # AI_SPHERE
AI_SPHERE_MSG_CENTER  # Mittelpunkt der Kugel
AI_SPHERE_MSG_RADIUS  # Radius für Kugel angeben oder [Durchmesser]
AI_SPHERE_MSG_DIAM    # Durchmesser der Kugel
AI_SPHERE_MSG_SURF1   # Anzahl der Längensegmente für Oberfläche der Kugel <16>
AI_SPHERE_MSG_SURF2   # Anzahl der Breitensegmente für Oberfläche der Kugel <16>
AI_SPHERE_KEY_DIAM    # Durchmesser

//----- AI_DOME
AI_DOME_CMD           # AI_DOME
AI_DOME_MSG_CENTER    # Mittelpunkt der Kuppel
AI_DOME_MSG_RADIUS    # Radius für Kuppel angeben oder [Durchmesser]
AI_DOME_MSG_DIAM      # Durchmesser der Kuppel
AI_DOME_MSG_SURF1     # Anzahl der Längensegmente für Oberfläche der Kuppel <16>
AI_DOME_MSG_SURF2     # Anzahl der Breitensegmente für Oberfläche der Kuppel <8>
AI_DOME_KEY_DIAM      # Durchmesser

//----- AI_DISH
AI_DISH_CMD           # AI_DISH
AI_DISH_MSG_CENTER    # Mittelpunkt der Schale
AI_DISH_MSG_RADIUS    # Radius für Schale angeben oder [Durchmesser]
AI_DISH_MSG_DIAM      # Durchmesser der Schale
AI_DISH_MSG_SURF1     # Anzahl der Längensegmente für Oberfläche der Schale <16>
AI_DISH_MSG_SURF2     # Anzahl der Breitensegmente für Oberfläche der Schale <8>
AI_DISH_KEY_DIAM      # Durchmesser

//----- AI_MESH
AI_MESH_CMD           # AI_MESH
AI_MESH_MSG_POINT1    # Ersten Eckpunkt für Netz angeben
AI_MESH_MSG_POINT2    # Zweiten Eckpunkt für Netz angeben
AI_MESH_MSG_POINT3    # Dritten Eckpunkt für Netz angeben
AI_MESH_MSG_POINT4    # Vierten Eckpunkt für Netz angeben
AI_MESH_MSG_SURFTAB1  # Größe des Netzes in M-Richtung eingeben
AI_MESH_MSG_SURFTAB2  # Größe des Netzes in N-Richtung eingeben

//----- AI_PYRAMID
AI_PYRAMID_CMD        # AI_PYRAMID
AI_PYRAMID_MSG_PT1    # Ersten Eckpunkt für Basis der Pyramide angeben
AI_PYRAMID_MSG_PT2    # Zweiten Eckpunkt für Basis der Pyramide angeben
AI_PYRAMID_MSG_PT3    # Dritten Eckpunkt für Basis der Pyramide angeben
AI_PYRAMID_MSG_PT4    # Vierten Eckpunkt für Basis der Pyramide angeben oder [Tetraeder]
AI_PYRAMID_MSG_PT5_TETRA   # Scheitelpunkt für Tetraeder angeben oder [Oberseite]
AI_PYRAMID_MSG_PT5_PYRAM   # Scheitelpunkt für Pyramide angeben oder [Kante/Oberseite]
AI_PYRAMID_MSG_PTOP1_TETRA # Erster Eckpunkt für Oberseite von Tetraeder angeben
AI_PYRAMID_MSG_PTOP2_TETRA # Zweiter Eckpunkt für Oberseite von Tetraeder angeben
AI_PYRAMID_MSG_PTOP3_TETRA # Dritter Eckpunkt für Oberseite von Tetraeder angeben
AI_PYRAMID_MSG_PTOP4_TETRA # Vierten Eckpunkt für Oberseite von Tetraeder angeben
AI_PYRAMID_MSG_PTOP1_PYRAM # Erster Eckpunkt für Oberseite von Pyramide angeben
AI_PYRAMID_MSG_PTOP2_PYRAM # Zweiter Eckpunkt für Oberseite von Pyramide angeben
AI_PYRAMID_MSG_PTOP3_PYRAM # Dritter Eckpunkt für Oberseite von Pyramide angeben
AI_PYRAMID_MSG_PTOP4_PYRAM # Vierten Eckpunkt für Oberseite von Pyramide angeben
AI_PYRAMID_MSG_PTOP1_RIDGE # Ersten Kantenendpunkt für Pyramide angeben
AI_PYRAMID_MSG_PTOP2_RIDGE # Zweiten Kantenendpunkt für Pyramide angeben
AI_PYRAMID_KEY_PYRAM  # Pyramide
AI_PYRAMID_KEY_TETRA  # Tetraeder
AI_PYRAMID_KEY_TOP    # Oberseite
AI_PYRAMID_KEY_RIDGE  # Kante

//----- AI_TORUS
AI_TORUS_CMD          # AI_TORUS
AI_TORUS_MSG_CENTER   # Torusmittelpunkt angeben
AI_TORUS_MSG_RADTOR   # Radius des Torus angeben oder [Durchmesser]
AI_TORUS_MSG_DIATOR   # Durchmesser des Torus angeben
AI_TORUS_MSG_RADTUBE  # Radius des Rings angeben oder [Durchmesser]
AI_TORUS_MSG_DIATUBE  # Durchmesser für Ring angeben
AI_TORUS_MSG_SURF1    # Anzahl der Segmente um den Ringumfang eingeben <16>
AI_TORUS_MSG_SURF2    # Anzahl der Segmente um den Torusumfang eingeben <16>
AI_TORUS_KEY_DIAM     # Durchmesser
AI_TORUS_ERR_DIAM     # Ringradius darf Torusradius nicht überschreiten.

//----- AI_WEDGE
AI_WEDGE_CMD          # AI_WEDGE
AI_WEDGE_MSG_CORNER   # Eckpunkt für Keil
AI_WEDGE_MSG_LENGTH   # Länge für Keil
AI_WEDGE_MSG_WIDTH    # Breite des Keil angeben
AI_WEDGE_MSG_HEIGHT   # Höhe für Keil
AI_WEDGE_MSG_ROTATE   # Drehwinkel des Keil um Z-Achse angeben oder [Referenz]

//----- SYSTEM VARIABLES
SYSVAR_Msg_ReadOnly     # (schreibgeschützt)
SYSVAR_Msg_Invalid      # *Ungültig*

SYSVAR_String_Msg       # Neuen Wert für %s eingeben <"%s">:
SYSVAR_String_MsgNoDef  # Neuen Wert für %s oder %s für keinen eingeben <"%s">:
SYSVAR_String_Err3      # Kann %s nicht auf diesen Wert setzen.

SYSVAR_Integer_Msg      # Neuen Wert für %s eingeben <%d>:
SYSVAR_Integer_Err1     # Erwartet nur %d oder %d Werte.
SYSVAR_Integer_Err2     # Wert muss eine Ganzzahl zwischen %d und %d sein.
SYSVAR_Integer_Err3     # Kann %s nicht auf diesen Wert setzen.
SYSVAR_Integer_Err4     # Zugelassene Werte für

SYSVAR_Real_Msg         # Neuen Wert für %s eingeben <%s>:
SYSVAR_Real_Err1        # Erwartet nur Werte im Bereich von %f bis %f.
SYSVAR_Real_Err3        # Kann %s nicht auf diesen Wert setzen.


