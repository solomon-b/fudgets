module EventMask where

-- Same order as in X
data EventMask = KeyPressMask |
                 KeyReleaseMask |
                 ButtonPressMask |
                 ButtonReleaseMask |
                 EnterWindowMask |
                 LeaveWindowMask |
                 PointerMotionMask |
                 PointerMotionHintMask |
                 Button1MotionMask |
                 Button2MotionMask |
                 Button3MotionMask |
                 Button4MotionMask |
                 Button5MotionMask |
                 ButtonMotionMask |
                 KeymapStateMask |
                 ExposureMask |
                 VisibilityChangeMask |
                 StructureNotifyMask |
                 ResizeRedirectMask |
                 SubstructureNotifyMask |
                 SubstructureRedirectMask |
                 FocusChangeMask |
                 PropertyChangeMask |
                 ColormapChangeMask |
                 OwnerGrabButtonMask 
                 deriving (Eq, Ord, Show, Read, Bounded, Enum)

clEventMask =
    [KeyPressMask, KeyReleaseMask, ButtonPressMask, ButtonReleaseMask, EnterWindowMask,
     LeaveWindowMask, PointerMotionMask, PointerMotionHintMask, Button1MotionMask,
     Button2MotionMask, Button3MotionMask, Button4MotionMask, Button5MotionMask,
     ButtonMotionMask, KeymapStateMask, ExposureMask, VisibilityChangeMask, StructureNotifyMask,
     ResizeRedirectMask, SubstructureNotifyMask, SubstructureRedirectMask, FocusChangeMask,
     PropertyChangeMask, ColormapChangeMask, OwnerGrabButtonMask]
