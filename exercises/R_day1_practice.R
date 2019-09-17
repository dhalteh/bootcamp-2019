


# Create a new variable that finds the natural log of the GDP per capita and of population - call them log_gdpPercap and log_pop
gapminder$log_gdpPercap <- log(gapminder$gdpPercap)
gapminder$logpop <- log(gapminder$pop)


years <- unique(gapminder$year)
for (year in years){
  year_mean <- mean(gapminder$lifeExp[gapminder$year == year], na.rm = T)
  print(paste0('The mean Life Expectacy in year ', year, ' is ', year_mean, ' years.'))
}


continents <- unique(gapminder$continent)
for(continent in continents){
  continent_mean <- mean(gapminder$lifeExp[gapminder$continent == continent], na.rm = T)
  print(paste0('The mean Life Expectancy in continent ', continent, ' is ', continent_mean, ' years.'))
}

# Nested for loop: continents and years
continents <- unique(gapminder$continent)
for(continent in continents){
  years <- unique(gapminder$year[gapminder$continent == continent])
  for(year in years){
    cy_mean <- mean(gapminder$lifeExp[gapminder$continent == continent & gapminder$year == year], na.rm = T)
    print(paste0('The mean Life Expectancy in continent ', continent, ' during the year ', year, ' is ', cy_mean, ' years.'))
  }
}


continents <- unique(gapminder$continent)
for(continent in continents){
  print(paste0('Continent: ', continent))
  years <- unique(gapminder$year[gapminder$continent == continent])
  for(year in years){
    sd_country_year <- sd(gapminder$lifeExp[gapminder$continent == continent & gapminder$year == year],
                       na.rm = T)
    print(paste0(year, ': ', sd_country_year))
  }
}


#Using apply -> apply(matrix, 1 = row OR 2 = column, function)
vars <- gapminder[, c('lifeExp', 'pop', 'gdpPercap')]
apply(vars, 2, mean)


#lapply
lapply(gapminder, mean)
lapply(years, function(x) mean(gapminder$lifeExp[gapminder$year == x])) #returns a list

#sapply
years <- unique(gapminder$year)
sapply(years, function(x) mean(gapminder$lifeExp[gapminder$year == x])) #returns a vector





i <- 1987

while(i <= 2002){
  sd_le <- sd(gapminder$lifeExp[gapminder$year == i])
  print(paste0(i, ': ', sd_le))
  i <- i + 5
}


for(i in unique(gapminder$year)){
  if(i >= 1987){
    print(paste0(i, ': ', mean(gapminder$pop[gapminder$year == i])))
  }else{
    print('Invalid year!')
  }
}



le_info <- function(continent){
  print(paste0('Mean Life Expectancy for ', continent, ' : ', mean(gapminder$lifeExp[gapminder$continent == continent],
                                                                   na.rm = T)))
  print(paste0('Mean Life Expectancy for ', continent, ' : ', median(gapminder$lifeExp[gapminder$continent == continent],
                                                                   na.rm = T)))
  print(paste0('Max. Life Expectancy for ', continent, ' : ', max(gapminder$lifeExp[gapminder$continent == continent],
                                                                  na.rm = T)))
  print(paste0('Min. Life Expectancy for ', continent, ' : ', min(gapminder$lifeExp[gapminder$continent == continent],
                                                                  na.rm = T)))
}

le_info('Americas')
















