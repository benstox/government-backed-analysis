#!/usr/bin/env python3.6
import pandas
import re

GOVERNMENT = (
    "Local and Regional Government",
    "Devolved Government",
    "Central Government")
FUND_TYPES = (
    "Private Equity and Venture Capital",
    "Corporate",
    "Angel Network",
    "Private Investment Vehicle",
    "Crowd funding")


def get_region(row):
    la = row.investee__company__head_office_local_authority
    if not pandas.isnull(la):
        return(regions.loc[regions.local_authority == la,"region"].values[0])
    elif row.investee__company__companies_house_id.lower().startswith("ni"):
        return("Northern Ireland")


def get_amount_gbp(row):
    conversion = row.amount_coalesced_currency__value
    amount = row.amount_coalesced_value
    if amount:
        return(conversion * amount)
    else:
        return(amount)


def get_government(fund_types):
    if not pandas.isnull(fund_types):
        fund_types = eval(fund_types)
        return(any(thing in GOVERNMENT for thing in fund_types))
    else:
        return(False)


def get_is_funded_by(fund_types, category):
    if not pandas.isnull(fund_types):
        fund_types = eval(fund_types)
        return(category in fund_types)
    else:
        return(False)


def get_company_sector(sectors, sector_group):
    if not pandas.isnull(sectors):
        selected_sectors = sectors_data.loc[sectors_data.sector_group == sector_group,"sector"]
        return(any(selected in sectors for selected in selected_sectors))
    else:
        return(False)


sectors_data = pandas.read_csv("sectors.csv", sep="|")
sectors_data = sectors_data.loc[~sectors_data.sector.str.contains("^Other")]
SECTOR_GROUPS = set(sectors_data.sector_group)
data = pandas.read_csv("fdbapp_fundraising_export.csv")
data.rename(
    columns={col_name: re.findall(r"\((.*)\)", col_name)[0] for col_name in data.columns.values},
    inplace=True)
regions = pandas.read_csv("regions.csv")
regions = regions.loc[regions.region != "England",:]
data["region"] = data.apply(get_region, axis=1)
data["amount_gbp"] = data.apply(get_amount_gbp, axis=1)
data["government"] = data["_participating_funds_fund_types"].apply(get_government)
for category in FUND_TYPES:
    category_col_name = category.lower().replace(" ", "_")
    data[category_col_name] = data["_participating_funds_fund_types"].apply(
        lambda x: get_is_funded_by(x, category))

for group in SECTOR_GROUPS:
    group_col_name = group.lower().replace(" ", "_").replace("/", "_").replace("-", "_").replace(",", "_")
    data[group_col_name] = data["investee__sectors"].apply(lambda x: get_company_sector(x, group))

data["is_dead"] = data["investee__company__current_stage_of_evolution"].apply(lambda x: x == "Dead")
data.drop([
    "_participating_funds",
    "_participating_funds_fund_types",
    "amount_coalesced_currency",
    "amount_coalesced_currency__value",
    "amount_coalesced_value",
    "investee__company__head_office_local_authority"], axis=1, inplace=True)

data.to_csv("for_analysis.csv")
