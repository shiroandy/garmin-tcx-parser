#garminread.R
-------------------------------------------------------------------------

__Prerequisites:__
- a working version of R (available at http://cran.r-project.org/)
- R library package "tcltk2" - this is simple tool for interactively choosing directories and files
- a .tcx file from a garmin device (this code has been tested with a tcx file from a Garmin Forerunner 310XT)

__Output:__
Dataframe in the following formats:
- .txt file (for easy access)
- .rdata (for use within R)

__Dataframe columns:__
 1. "lap" - points at which lap marker was pressed on watch  
 2. "time" - date and time in standard format  
 3. "UNIXtime" - UNIX time      
 4. "hrbpm" - heart rate (beats per minute) 
 5. "hrbpmx" - interpolated heart rate 
 6. "lat" - latitude  
 7. "latx" - interpolated latitude  
 8. "lon" - longitude  
 9. "lonx" - interpolated longitude  
 10. "alt" - altitude  
 11. "dist" - cumulative distance (meters)  
 12. "dist_interpol" - interpolated distance  
 13. "p2p_dist" - point to point distance (from previous to current point)  
 14. "tot_dist" - cumulative distance interpolated  
 15. "speed" - speed (km/hr)  



-------------------------------------------------------------------------
Please direct comments/queries to Andrew J. White - shiroandy@gmail.com

