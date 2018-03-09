select grid.gridCode, region.regionName, grid.longitude, grid.latitude, countryCode, countryName, eventYear, eventName, syndrome.syndromeName, eventDate, initialDate, finalDate, days, months from grid 
inner join event_has_grid eg on eg.`grid_gridCode` = grid.gridCode 
left join event on event.`id_events` = eg.`event_id_events` 
left join event_has_syndrome es on es.`event_id_events` = event.`id_events` 
left join syndrome on syndrome.syndromeID = es.`syndrome_syndromeID` 
left join grid_isin_region gr on gr.gridCode = grid.gridCode 
left join country on country.countryID = grid.countryCode 
left join region on gr.regionID = region.regionID