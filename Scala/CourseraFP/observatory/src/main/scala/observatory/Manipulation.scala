package observatory

import Visualization.predictTemperature
/**
  * 4th milestone: value-added information
  */
object Manipulation {


  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature = {
    def tempAtGridLocation(gridLoc: GridLocation, temps: Iterable[(Location, Temperature)]): Temperature = {
      val location = Location(gridLoc.lat.toDouble, gridLoc.lon.toDouble)
      predictTemperature(temps, location)
    }

    val gridLocations = Array.tabulate[GridLocation](180,360){
      (iLat, iLon) => new GridLocation(iLat-89, iLon-180)
    }
    val gridTemperatures = gridLocations.map(_.map(tempAtGridLocation(_, temperatures)))

    def tempAtGridLoc(gridLoc: GridLocation): Temperature = {
      gridTemperatures(gridLoc.lat+89)(gridLoc.lon+180)
    }
    tempAtGridLoc
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature = {
    val tempGrids: Iterable[GridLocation => Temperature] = temperaturess.map(makeGrid(_))

    def avgTempAtGridLoc(gridLoc: GridLocation): Temperature = {
      val tempsAtGridLoc = tempGrids.map(_(gridLoc))
      val sumTemps: Temperature = tempsAtGridLoc.reduce(_+_)
      val nTemps: Double = tempsAtGridLoc.size.toDouble
      sumTemps/nTemps
    }
    avgTempAtGridLoc
  }

  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation => Temperature = {
    val gridTemps: GridLocation => Temperature = makeGrid(temperatures)

    def devTemp(gridLoc: GridLocation): Temperature = {
      gridTemps(gridLoc) - normals(gridLoc)
    }
    devTemp
  }


}

