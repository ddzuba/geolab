module GeolibGlobals
use dflogm
use gl_core
implicit none

	INTEGER*4  gval /1/

	integer dllHandler

	integer mainHWND

	character(255) targetFolder/""C/

	integer isStarted /0/
    
    integer isWindow /0/
    
    type(T_MISC_GEO_DATA), pointer :: pMainGeoData;

end module GeolibGlobals