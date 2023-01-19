# Regression workflow and documentation
# keaton@virgalabs.io
# 2023-01-19

# generally this workflow is designed to outline and document building predictive
# regression models to convert live storage from the BoR Hydrodata API
# to pool elevations for both Lake Mead and Lake Powell

# These models will be used in the Colorado River Tracker to generate projected
# elevation levels based on projected storage volumes

# library(tidyverse)
# library(lubridate)
# library(broom)

# # get data
# powell_elev_data = get_res_data(919, 49)
# powell_storage_data = get_res_data(919, 17)
# mead_elev_data = get_res_data(921, 49)
# mead_storage_data = get_res_data(921, 17)
#
# # combine
# powell = left_join(powell_elev_data, powell_storage_data) |> rename(pool_elevation = `pool elevation`)
# mead = left_join(mead_elev_data, mead_storage_data) |> rename(pool_elevation = `pool elevation`)
#
# # powell graph
# powell |>
#   ggplot(aes(x = storage, y = pool_elevation)) +
#   geom_point()
#
# # mead graph
# mead |>
# ggplot(aes(x = storage, y = pool_elevation)) +
#   geom_point()
#
# # model polynomial
# powell_mod = lm(pool_elevation ~ poly(storage, 2), data = powell)
# mead_mod = lm(pool_elevation ~ poly(storage,2), data = mead |> drop_na())
#
# # summaries - pretty good on both
# summary(powell_mod)
# summary(mead_mod)
# tidy(powell_mod)
# glance(powell_mod)

