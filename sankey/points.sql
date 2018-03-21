select ronname, round(points.longitude::numeric, 2), round(points.latitude::numeric, 2), count(*) as records 
from explore.points
left join obis.resources on points.resource_id = resources.id
left join obis.rons on rons.id = resources.ron_id
group by ronname, round(points.longitude::numeric, 2), round(points.latitude::numeric, 2);