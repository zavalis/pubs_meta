import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker
import matplotlib.lines as mlines
import seaborn as sns
from statsmodels import robust 


environmental_and_occupational_risks=['Unsafe water, sanitation, and handwashing',
                                      'Unsafe water and sanitation',
                                      'Air pollution',
                                      'Non-optimal temperature',
                                      'Other environmental risks',
                                      'Occupational risks']

behavioural_risks=['Child and maternal malnutrition', 
                   'Tobacco',
                   'High alcohol use',
                   'Drug use',
                   'Dietary risks',
                   'Intimate partner violence',
                   'Unsafe sex',
                   'Low physical activity'
                   ]

metabolic_risks=['High fasting plasma glucose',
                 'High LDL cholesterol*',
                 'High systolic blood pressure',
                 'High body-mass index',
                 'Low bone mineral density',
                 'Kidney dysfunction*'
                 ]

overarching_risks = [
    'Environmental/occupational risks',
    'Behavioural risks',
    'Metabolic risks',
    'All risk factors'
]

# Environmental risks
wata = ['Unsafe water source','Unsafe sanitation', 'No access to handwashing facility'] # are there not more? unsafe water source?
pollu = ['Ambient particulate matter pollution', 'Ambient ozone pollution', 'Household air pollution from solid fuels', 'Particulate matter pollution', 'Nitrogen dioxide pollution']
temp = ['Low temperature', 'High temperature']
otherenvir = ['Residential radon', 'Lead exposure']
occupation = ['Occupational carcinogens', 'Occupational asthmagens','Occupational particulate matter, gases, and fumes', 'Occupational noise', 'Occupational injuries', 'Occupational ergonomic factors']

# Behavioural risks 
childfood = ['Childhood undernutrition', 'Zinc deficiency', 'Iron deficiency', 'Vitamin A deficiency', 'Child growth failure', 'Low birth weight and short gestation', 'Suboptimal breastfeeding']
tobacco = ['Smoking', 'Smokeless tobacco', 'Chewing tobacco', 'Second-hand smoke']
# childabuse is removed ...
childabuse = [ 'Childhood sexual abuse','Childhood sexual abuse and bullying', 'Sexual violence against children and bullying', 'Sexual abuse and violence', 'Sexual violence against children', 'Childhood maltreatment', 'Bullying victimization', 'Sexual violence and bullying victimization']

dietary_risks=['Diet low in fruits', 'Diet low in vegetables', 'Diet low in whole grains', 'Diet low in nuts and seeds', 'Diet low in milk', 'Diet low in fiber', 'Diet low in calcium', 'Diet low in seafood omega-3 fatty acids', 'Diet low in \npolyunsaturated fatty acids', 'Diet low in legumes', 'Diet high in red meat', 'Diet high in processed meat', 'Diet high in \nsugar sweetened beverages', 'Diet high in trans fatty acids', 'Diet high in sodium','Low physical activity']

def plot_gbd(df, measure="Deaths", risk_subset=None,
             special_colors=None, default_color="#999999",
             ncols=1, figsize=(10, 15), save_path=None):
    """
    Panel plot of GBD estimates by Risk_factor and year_gbd.
    
    Parameters
    ----------
    df : pd.DataFrame
        Must include columns 'Risk_factor', 'year_gbd', 'anal_year', 'measure', 'val'.
    measure : str
        Which measure to plot (e.g., 'Deaths', 'DALYs').
    risk_subset : list[str] or None
        If provided, only plot these risk factors.
    special_colors : dict or None
        Mapping from year_gbd -> color.
    default_color : str
        Default color for missing years.
    ncols : int
        Number of subplot columns.
    figsize : tuple
        Figure size.
    save_path : str or None
        Path to save figure.
    """
    df = df.copy()
    df['year_gbd'] = df['year_gbd'].astype(int)

    #df = df[df['measure'] == measure]
    
    if risk_subset is not None:
        df = df[df["Risk_factor"].isin(risk_subset)]
    
    if special_colors is None:
        special_colors = {2021: "#4E79A7", 2019: "#E15759",
                          2017: "#999999", 2016: "#999999",
                          2015: "#999999", 2013: "#999999", 2010: "#999999"}
    
    df["color"] = df["year_gbd"].map(special_colors).fillna(default_color)
    
    # Sort risk factors
    df = df.sort_values(["Overarching_group", "Risk_factor"])
    risk_factors = df["Risk_factor"].unique()
    
    nrows = int(np.ceil(len(risk_factors) / ncols))
    fig, axes = plt.subplots(nrows, ncols, figsize=figsize, sharex=True, sharey=True)
    axes = axes.flatten()
    
    for idx, risk in enumerate(risk_factors):
        ax = axes[idx]
        risk_data = df[df["Risk_factor"] == risk].copy()
        #risk_data["val_million"] = risk_data["val"] / 1_000_000
        year_order = sorted(risk_data['year_gbd'].unique())

        sns.pointplot(
            data=risk_data,
            x="anal_year",
            y="val",
            hue="year_gbd",
            hue_order=year_order,
            dodge=True,
            markers='o',
            linestyles='None',
            palette=special_colors,
            #errorbar=('ci', 95),
            ax=ax
        )
        
        # Cleanup
        ax.spines['top'].set_visible(False)
        ax.spines['right'].set_visible(False)
        ax.set_xlabel("")
        ax.set_ylabel(f"{measure} (M)")
        ax.set_title(risk, loc='left', fontsize=16, fontweight='bold')
        
        #ax.set_ylim(0, risk_data["val"].max() * 1.1)
        ax.yaxis.set_major_formatter(mticker.FuncFormatter(lambda x, _: f'{int(x*1e-6)}M'))
        if ax.get_legend() is not None:
            ax.legend_.remove()
    
    # Remove unused axes
    for j in range(len(risk_factors), len(axes)):
        fig.delaxes(axes[j])
    
    # Custom legend
    handles = [
        mlines.Line2D([], [], marker='o', color='w', markerfacecolor=color, markersize=10, label=str(year))
        for year, color in special_colors.items()
    ]
    
    fig.legend(handles, list(special_colors.keys()), title='GBD Iteration',
               loc='center left', bbox_to_anchor=(0.95, 0.5),
               prop={'size': 12}, title_fontsize=14)
    
    plt.tight_layout(rect=[0, 0, 0.9, 1])
    
    if save_path is not None:
        plt.savefig(save_path, bbox_inches='tight')
    
    plt.show()

import matplotlib.pyplot as plt
from matplotlib.ticker import FuncFormatter

def plot_risk_factors(df, measure, filename, nrows=4, ncols=4, y_tick_step=None):
    """
    Panel plot for GBD risk factors over years.

    Parameters
    ----------
    df : pd.DataFrame
        Must have columns 'Risk_factor', 'year_gbd', 'measure', 'val'.
    measure : str
        Which measure to plot ('Deaths', 'DALYs', etc.).
    filename : str
        Path to save the figure.
    nrows, ncols : int
        Grid dimensions.
    y_tick_step : int or None
        Step size for horizontal dashed lines and y-axis ticks.
    """
    plt.style.use('seaborn-v0_8-colorblind')
    plt.rcParams.update({'font.size': 12})

    df_plot = df[df['measure'] == measure]

    categories = sorted(df_plot['Risk_factor'].unique())

    # Custom formatter (same for all)
    formatter = FuncFormatter(lambda x, _: f'{x / 1_000_000:.0f}M')

    fig, axes = plt.subplots(nrows=nrows, ncols=ncols, figsize=(12, 12), sharex=True, sharey=True)

    # Set y-axis dashed lines
    if y_tick_step is None:
        y_max = df_plot['val'].max()
        y_tick_step = y_max // 6  # default 6 ticks
    dash_lines = list(range(0, int(df_plot['val'].max()) + y_tick_step, y_tick_step))

    for ax, category in zip(axes.flat, categories):
        subset = df_plot[df_plot['Risk_factor'] == category]
        ax.plot(subset['year_gbd'], subset['val'], marker='o', linestyle='-', linewidth=1, markersize=5)

        ax.set_title(category, fontsize=10)
        ax.set_xticks([2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021])
        ax.set_xticklabels(['2010','','','2013','','2015','2016','2017','','2019','','2021'], rotation=45, ha='center')
        ax.set_yticks(dash_lines)
        ax.yaxis.set_major_formatter(formatter)
        ax.spines['top'].set_visible(False)
        ax.spines['right'].set_visible(False)

        for y in dash_lines:
            ax.axhline(y=y, color='black', linestyle='dashed', alpha=0.4)

    plt.tight_layout()
    plt.savefig(filename, bbox_inches='tight')
    plt.show()

def plot_risk_factors_side_by_side(df, measures, filename, nrows=4, ncols=4, y_tick_steps=None):
    """
    Plot risk factors for multiple measures side by side.
    """
    plt.style.use('seaborn-v0_8-colorblind')
    plt.rcParams.update({'font.size': 12})

    if y_tick_steps is None:
        y_tick_steps = [None] * len(measures)

    fig, big_axes = plt.subplots(1, len(measures), figsize=(len(measures) * 12, 12))

    # Make sure we can iterate over axes
    if len(measures) == 1:
        big_axes = [big_axes]

    for ax, measure, y_tick_step in zip(big_axes, measures, y_tick_steps):
        df_plot = df[df['measure'] == measure]
        categories = sorted(df_plot['Risk_factor'].unique())

        formatter = FuncFormatter(lambda x, _: f'{x / 1_000_000:.0f}M')
        subfig = ax.figure
        subfig.suptitle('')  # no super title in subfig

        # Create a small grid inside each main axis
        subaxes = ax.inset_axes([0, 0, 1, 1]).subplots(
            nrows=nrows, ncols=ncols, sharex=True, sharey=True
        )

        if y_tick_step is None:
            y_max = df_plot['val'].max()
            y_tick_step = y_max // 6
        dash_lines = list(range(0, int(df_plot['val'].max()) + y_tick_step, y_tick_step))

        for a, category in zip(subaxes.flat, categories):
            subset = df_plot[df_plot['Risk_factor'] == category]
            a.plot(subset['year_gbd'], subset['val'], marker='o', linewidth=1, markersize=5)
            a.set_title(category, fontsize=10)
            a.set_xticks([2010,2013,2015,2016,2017,2019,2021])
            a.set_xticklabels(['2010','2013','2015','2016','2017','2019','2021'], rotation=45)
            a.set_yticks(dash_lines)
            a.yaxis.set_major_formatter(formatter)
            a.spines['top'].set_visible(False)
            a.spines['right'].set_visible(False)
            for y in dash_lines:
                a.axhline(y=y, color='black', linestyle='dashed', alpha=0.4)

        ax.set_title(measure, fontsize=16, pad=20)
        plt.tight_layout()

    plt.savefig(filename, bbox_inches='tight')
    plt.show()

def compare_within_group(group):
    values = group[['val', 'lower', 'upper']].values
    val_col = values[:, 0]
    lower_col = values[:, 1]
    upper_col = values[:, 2]

    n = len(val_col)
    unique_comparisons = ((n * (n - 1)) // 2)*2

    count_outside = 0
    count_outside_100k = 0
    count_outside_1m = 0

    for i in range(n):
        for j in range(i + 1, n):
            # Check val[i] against interval of j
            if val_col[i] < lower_col[j] or val_col[i] > upper_col[j]:
                abs_diff_ij = abs(val_col[i] - (lower_col[j] if val_col[i] < lower_col[j] else upper_col[j]))
                count_outside += 1
                if abs_diff_ij > 100000:
                    count_outside_100k += 1
                if abs_diff_ij > 1000000:
                    count_outside_1m += 1

            # Check val[j] against interval of i
            if val_col[j] < lower_col[i] or val_col[j] > upper_col[i]:
                abs_diff_ji = abs(val_col[j] - (lower_col[i] if val_col[j] < lower_col[i] else upper_col[i]))
                count_outside += 1
                if abs_diff_ji > 100000:
                    count_outside_100k += 1
                if abs_diff_ji > 1000000:
                    count_outside_1m += 1

    return pd.Series({
        'UniqueComparisons': unique_comparisons,
        'CountOutside': count_outside,
        'CountOutside_100k': count_outside_100k,
        'CountOutside_1m': count_outside_1m
    })

    
 
# Assign subgroups (fine level)
def assign_grouping(rf):
    if rf in metabolic_risks:
        return rf  # use individual metabolic risks as their subgroup
    elif rf in environmental_and_occupational_risks:
        return rf
    elif rf in behavioural_risks:
        return rf
    elif rf in childfood:
        return 'Child and maternal malnutrition'
    elif rf in tobacco:
        return 'Tobacco'
    elif rf in childabuse:
        return 'Childhood sexual abuse'
    elif rf in dietary_risks:
        return 'Dietary risks'
    for groupname, members in {
        'Unsafe water, sanitation, and handwashing': wata,
        'Air pollution': pollu,
        'Non-optimal temperature': temp,
        'Other environmental risks': otherenvir,
        'Occupational risks': occupation
    }.items():
        if rf in members:
            return groupname
    return 'Other'

# Assign overarching grouping
def assign_overarching(group):
    if group in metabolic_risks:
        return 'Metabolic risks'
    elif group in behavioural_risks:
        return 'Behavioural risks'
    elif group in environmental_and_occupational_risks:
        return 'Environmental/occupational risks'
    elif group in ['Other']:
        return group
    
    return 'Other'

# Define function to check interval position
def interval_position(row):
    if row['lower'] <= row['val_ref'] <= row['upper']:
        return "within"
    elif row['val_ref'] < row['lower']:
        return "below"
    elif row['val_ref'] > row['upper']:
        return "above"
    return 'WHAT HAPPENED HERE'

# Define function to check interval overlap
def intervals_overlap(row):
    if row['upper_ref'] < row['lower']:
        return 'no overlap - reference below'
    elif row['upper'] < row['lower_ref']:
        return 'no overlap - reference above'
    return 'overlap'


def summarize_deaths_dalys_full(df, value_col='val', min_year=None, exclude_group=None):
    """
    Summarizes Deaths and DALYs per risk factor, group, and optionally analysis year.
    Returns two DataFrames:
      1. Year-specific summary (grouped by anal_year)
      2. Aggregated summary (across all years)
    
    Parameters:
        df: pandas DataFrame with columns ['Overarching_group','Group','Risk_factor','measure','anal_year',value_col]
        value_col: column to summarize (default 'val')
        min_year: optional minimum anal_year to filter
        exclude_group: optional group to exclude (e.g., 'Other')
    """
    
    # Optional year filter
    if min_year is not None:
        df = df[df['anal_year'] > min_year]
    
    # Optional exclusion
    if exclude_group is not None:
        df = df[df['Group'] != exclude_group]
    
    # Internal function to summarize a single measure
    # for background on estimators of variability, see:
    # https://pmc.ncbi.nlm.nih.gov/articles/PMC9196089/

    def summarize_metric(sub_df, group_cols):
        summary = (
            sub_df.groupby(group_cols)[value_col]
            .agg(
                median='median', 
                min='min', 
                max='max', 
                count='count',
                mean='mean', 
                std='std',
                # custom functions
                
                # gioli= f_med=max(max/median, median/min)
                mad=lambda x: robust.mad(x, c=1)
            )
            .reset_index()
        )
        summary['range'] = summary['max'] - summary['min']
        summary['fold_range_vs_median'] = summary['range'] / summary['median']
        summary['rmu'] = summary['range'] / summary['mean']

        summary['cv']=summary['std']/summary['mean']
        summary['max_vs_min'] = summary['max'] / summary['min']
        summary['f_med']=np.maximum(summary['max']/summary['median'], summary['median']/summary['min'])
        
        # robust CV is RCV_M=1.482 * MAD/median (Ampel JASA 1974)
        summary['rcv_m'] = 1.482 * summary['mad'] / summary['median']  # robust CV
        
        # median modified Miller interval [Miller E.G., Commun. Stat. A - Theory Methods 20 (1991), pp. 3351–3363]
        # insanely unstable in this case actually, so not using it
        # median modification of the modified McKay, Panich method and Gulhar methods are all parametric so not that interesting
        # and use chi squared distribution approximations that ont apply here
        summary = summary.rename(columns={'count': 'n_estimates'})
        return summary
    
    # ----------------------
    # 1. Year-specific summary
    group_cols_year = ['Overarching_group','Group','Risk_factor','measure','anal_year']
    deaths_year = summarize_metric(df[df['measure']=='Deaths'], group_cols_year)
    dalys_year  = summarize_metric(df[df['measure']=='DALYs'], group_cols_year)
    
    # Rename columns
    deaths_year = deaths_year.rename(
        columns={col: f"{col}_Deaths" for col in deaths_year.columns 
                 if col not in ['Overarching_group','Group','Risk_factor','anal_year','measure']}
    )

    dalys_year = dalys_year.rename(
        columns={col: f"{col}_DALYs" for col in dalys_year.columns 
                 if col not in ['Overarching_group','Group','Risk_factor','anal_year','measure']}
    )
    
    summary_wide_year = pd.merge(
        deaths_year.drop(columns='measure'),
        dalys_year.drop(columns='measure'),
        on=['Overarching_group','Group','Risk_factor','anal_year'],
        how='outer'
    )
    
    # ----------------------
    # 2. Aggregated summary (across all years)
    group_cols_agg = ['Overarching_group','Group','Risk_factor','measure']
    deaths_agg = summarize_metric(df[df['measure']=='Deaths'], group_cols_agg)
    dalys_agg  = summarize_metric(df[df['measure']=='DALYs'], group_cols_agg)
    
    deaths_agg = deaths_agg.rename(
        columns={col: f"{col}_Deaths" for col in deaths_agg.columns 
                 if col not in ['Overarching_group','Group','Risk_factor','measure']}
    )
    dalys_agg = dalys_agg.rename(
        columns={col: f"{col}_DALYs" for col in dalys_agg.columns 
                 if col not in ['Overarching_group','Group','Risk_factor','measure']}
    )
    
    summary_wide_agg = pd.merge(
        deaths_agg.drop(columns='measure'),
        dalys_agg.drop(columns='measure'),
        on=['Overarching_group','Group','Risk_factor'],
        how='outer'
    )
    
    return summary_wide_year, summary_wide_agg

def compute_proportions(summary_wide, output_csv=None):
    """
    Compute proportions and counts of fold range vs mean > 1
    at both overarching and subgroup levels and return a formatted DataFrame.

    Parameters:
        summary_wide: DataFrame with columns:
            ['Overarching_group','Group','Risk_factor','rmu_Deaths','rmu_DALYs']
        output_csv: optional path to save the formatted table

    Returns:
        formatted_df: DataFrame with DALYs and Deaths formatted columns
    """
    # 1. Proportions at overarching level
    prop_overarching = (
        summary_wide
        .groupby('Overarching_group')
        .agg(
            prop_deaths_gt1=('rmu_Deaths', lambda x: (x > 1).mean()),
            prop_dalys_gt1=('rmu_DALYs', lambda x: (x > 1).mean()),
            n_risk_factors=('Risk_factor', 'count'),
            count_deaths_gt1=('rmu_Deaths', lambda x: (x > 1).sum()),
            count_dalys_gt1=('rmu_DALYs', lambda x: (x > 1).sum()),
        )
        .reset_index()
    )

    # 2. Proportions at subgroup level
    prop_subgroups = (
        summary_wide
        .groupby(['Overarching_group', 'Group'])
        .agg(
            prop_deaths_gt1=('rmu_Deaths', lambda x: (x > 1).mean()),
            prop_dalys_gt1=('rmu_DALYs', lambda x: (x > 1).mean()),
            n_risk_factors=('Risk_factor', 'count'),
            count_deaths_gt1=('rmu_Deaths', lambda x: (x > 1).sum()),
            count_dalys_gt1=('rmu_DALYs', lambda x: (x > 1).sum()),
        )
        .reset_index()
    )

    # 3. Format DALYs and Deaths columns
    for df in [prop_subgroups, prop_overarching]:
        df['DALYs'] = df['count_dalys_gt1'].astype(str) + '/' + df['n_risk_factors'].astype(str) + \
                      ' (' + df['prop_dalys_gt1'].round(2).astype(str) + ')'
        df['Deaths'] = df['count_deaths_gt1'].astype(str) + '/' + df['n_risk_factors'].astype(str) + \
                       ' (' + df['prop_deaths_gt1'].round(2).astype(str) + ')'

    # 4. Combine and sort
    formatted_df = pd.concat([
        prop_subgroups[['Overarching_group', 'Group', 'DALYs', 'Deaths']],
        prop_overarching[['Overarching_group', 'DALYs', 'Deaths']]
    ]).sort_values(by=['Overarching_group', 'Group'], na_position='first')

    # 5. Save CSV if requested
    if output_csv is not None:
        formatted_df.to_csv(output_csv, index=False)

    return formatted_df

def format_and_pivot_summary(summary_df, value_cols=('Deaths','DALYs'), scale_cols=1000):
    """
    Formats summary metrics and pivots the table by 'anal_year' for specified value columns.

    Parameters:
        summary_df: DataFrame containing summary metrics including:
            ['anal_year','Risk_factor', 'median_Deaths','min_Deaths','max_Deaths',
             'rmu_Deaths','median_DALYs','min_DALYs','max_DALYs',
             'rmu_DALYs']
        value_cols: tuple of value columns to format ('Deaths', 'DALYs')
        scale_cols: scale factor for rounding counts (e.g., 1000)
    
    Returns:
        pivoted_df: DataFrame pivoted with Risk_factor as index, anal_year as columns, 
                    and formatted DALYs/Deaths as values.
    """
    
    # Define rounding columns
    columns_to_round_whole = [f'median_{v}' for v in value_cols] + \
                             [f'min_{v}' for v in value_cols] + \
                             [f'max_{v}' for v in value_cols] + \
                             [f'range_{v}' for v in value_cols]
    columns_to_round_float = [f'rmu_{v}' for v in value_cols]

    # Round whole-number columns (scaled down)
    for col in columns_to_round_whole:
        if col in summary_df.columns:
            summary_df[col] = summary_df[col].apply(lambda x: int(round(x/scale_cols, 0)) if pd.notna(x) else x)
            summary_df[col] = pd.to_numeric(summary_df[col], errors='coerce').astype('Int64')

    # Round fold_range columns to 1 decimal
    for col in columns_to_round_float:
        if col in summary_df.columns:
            summary_df[col] = summary_df[col].apply(lambda x: round(x, 1) if pd.notna(x) else x)

    # Format the values as "median (min-max) [fold_range]"
    for v in value_cols:
        summary_df[v] = summary_df.apply(
            lambda row: f"{row[f'median_{v}']} ({row[f'min_{v}']}-{row[f'max_{v}']}) [{row[f'rmu_{v}']:.1f}]"
            if pd.notna(row[f'median_{v}']) else None,
            axis=1
        )

    # Prepare subset for pivot
    summary_subset = summary_df[['anal_year','Risk_factor'] + list(value_cols)].copy()

    # Pivot each value column separately
    pivoted_dict = {}
    for v in value_cols:
        pivoted_dict[v] = summary_subset.pivot(index='Risk_factor', columns='anal_year', values=v)

    return pivoted_dict

