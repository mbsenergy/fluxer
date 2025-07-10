from entsoe import EntsoePandasClient
import pandas as pd

ENTSOE_KEY = "4e2c5951-f4e9-48f6-92af-b083178b0a89"

start = pd.Timestamp("20170101", tz="Europe/Brussels")
end = pd.Timestamp("20170201", tz="Europe/Brussels")
country_code = "IT_CSUD"

# methods that return Pandas Series
client = EntsoePandasClient(api_key=ENTSOE_KEY)
# df_dam = client.query_day_ahead_prices(country_code, start=start, end=end)
# df_dam = df_dam.reset_index()
# df_dam.columns = ['datetime', 'price']
# df_dam['date'] = df_dam['datetime'].dt.date
# df_dam['hour'] = df_dam['datetime'].dt.hour
# df_dam = df_dam[['date', 'hour', 'price']]


df_gen = client.query_generation(country_code, start=start, end=end, psr_type=None)
idx = pd.IndexSlice
df_agg = df_gen.loc[:, idx[:, "Actual Aggregated"]]
df_agg.columns = df_agg.columns.droplevel(1)
df_agg = df_agg.reset_index()
df_long = df_agg.melt("index", var_name="generation_type", value_name="value")
df_long

df_cap = client.query_installed_generation_capacity(
    country_code, start=start, end=end, psr_type=None
)
df_loa = client.query_load_and_forecast(country_code, start=start, end=end)
df_phy_im = client.query_physical_crossborder_allborders(
    country_code, start, end, export=False
)
df_phy_ex = client.query_physical_crossborder_allborders(
    country_code, start, end, export=True
)


# client.query_net_position(country_code, start=start, end=end, dayahead=True)
# client.query_crossborder_flows(country_code_from, country_code_to, start=start, end=end)
# client.query_scheduled_exchanges(country_code_from, country_code_to, start=start, end=end, dayahead=False)
# client.query_net_transfer_capacity_dayahead(country_code_from, country_code_to, start=start, end=end)
# client.query_net_transfer_capacity_weekahead(country_code_from, country_code_to, start=start, end=end)
# client.query_net_transfer_capacity_monthahead(country_code_from, country_code_to, start=start, end=end)
# client.query_net_transfer_capacity_yearahead(country_code_from, country_code_to, start=start, end=end)
# client.query_intraday_offered_capacity(country_code_from, country_code_to, start=start, end=end, implicit=True)
# client.query_offered_capacity(country_code_from, country_code_to, contract_marketagreement_type, start=start, end=end, implicit=True)
# client.query_aggregate_water_reservoirs_and_hydro_storage(country_code, start=start, end=end)
# client.query_load(country_code, start=start, end=end)
# client.query_load_forecast(country_code, start=start, end=end)
# client.query_generation_forecast(country_code, start=start, end=end)
# client.query_wind_and_solar_forecast(country_code, start=start, end=end, psr_type=None)
# client.query_intraday_wind_and_solar_forecast(country_code, start=start, end=end, psr_type=None)
# client.query_generation_per_plant(country_code, start=start, end=end, psr_type=None, include_eic=False)
# client.query_installed_generation_capacity_per_unit(country_code, start=start, end=end, psr_type=None)
# client.query_imbalance_prices(country_code, start=start, end=end, psr_type=None)
# client.query_contracted_reserve_prices(country_code, type_marketagreement_type, start=start, end=end, psr_type=None)
# client.query_contracted_reserve_amount(country_code, type_marketagreement_type, start=start, end=end, psr_type=None)
# client.query_unavailability_of_generation_units(country_code, start=start, end=end, docstatus=None, periodstartupdate=None, periodendupdate=None)
# client.query_unavailability_of_production_units(country_code, start, end, docstatus=None, periodstartupdate=None, periodendupdate=None)
# client.query_unavailability_transmission(country_code_from, country_code_to, start=start, end=end, docstatus=None, periodstartupdate=None, periodendupdate=None)
# client.query_withdrawn_unavailability_of_generation_units(country_code, start, end)
# client.query_unavailability_of_offshore_grid(area_code, start, end)
# client.query_generation_import(country_code, start, end)
# client.query_procured_balancing_capacity(country_code, process_type, start=start, end=end, type_marketagreement_type=None)
