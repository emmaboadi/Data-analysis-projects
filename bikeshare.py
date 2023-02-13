import time
import pandas as pd
import numpy as np

CITY_DATA = { 'chicago': 'chicago.csv',
              'new york city': 'new_york_city.csv',
              'washington': 'washington.csv' }

def get_filters():
    """
    Asks user to specify a city, month, and day to analyze.

    Returns:
        (str) city - name of the city to analyze
        (str) month - name of the month to filter by, or "all" to apply no month filter
        (str) day - name of the day of week to filter by, or "all" to apply no day filter
    """
    print('Hello! Let\'s explore some US bikeshare data!')
    # TO DO: get user input for city (chicago, new york city, washington). HINT: Use a while loop to handle invalid inputs
    city = input('Enter Your City: ').lower()
    while city not in ['chicago', 'new york city', 'washington']:
        city = input ('Kindly Choose between; chicago, new york city OR washington: ').lower()
        

    # TO DO: get user input for month (all, january, february, ... , june)
    month = input('Enter month: ').lower()
    while month not in ['all','january','febuary','march','april','may','june']:
        month = input ('Kindly Enter A Month From January to June!: ').lower()
     

    # TO DO: get user input for day of week (all, monday, tuesday, ... sunday)
    day = input('Enter Day of the Week: ').lower()

    print('-'*40)
    return city, month, day


def load_data(city, month, day):
    """
    Loads data for the specified city and filters by month and day if applicable.

    Args:
        (str) city - name of the city to analyze
        (str) month - name of the month to filter by, or "all" to apply no month filter
        (str) day - name of the day of week to filter by, or "all" to apply no day filter
    Returns:
        df - Pandas DataFrame containing city data filtered by month and day
    """
    df = pd.read_csv('chicago.csv')
    df = pd.read_csv('new_york_city.csv')
    df = pd.read_csv('washington.csv')
    
    df['Start Time'] = pd.to_datetime(df['Start Time'])
    df['End Time'] = pd.to_datetime(df['End Time'])
    
    df['month'] = df['Start Time'].dt.month
    
    if month != 'all':
        months = ['january', 'february', 'march', 'april', 'may', 'june']
        month = months.index(month) + 1

        df = df[df['month'] == month]

    
    df['day_of_week'] = df['Start Time'].dt.weekday_name
   
    if day != 'all':
        df = df[df['day_of_week'] == day.title()]
    
    

    return df


def time_stats(df):
    """Displays statistics on the most frequent times of travel."""

    print('\nCalculating The Most Frequent Times of Travel...\n')
    start_time = time.time()

    # TO DO: display the most common month
    print('The most common month is: ', df['month'].value_counts().idxmax())

    # TO DO: display the most common day of week
    df['day_of_week'] = df['Start Time'].dt.weekday_name
    
    print('The most common day of the week is: ', df['day_of_week'].value_counts().idxmax())

    # TO DO: display the most common start hour
    df['hour'] = df['Start Time'].dt.hour
    
    print('The most common hour is: ', df['hour'].value_counts().idxmax())

    print("\nThis took %s seconds." % (time.time() - start_time))
    print('-'*40)


def station_stats(df):
    """Displays statistics on the most popular stations and trip."""

    print('\nCalculating The Most Popular Stations and Trip...\n')
    start_time = time.time()

    # TO DO: display most commonly used start station
    print('The most commonly used start station is: ', df['Start Station'].value_counts().idxmax())

    # TO DO: display most commonly used end station
    print('The most common used end station is: ', df['End Station'].value_counts().idxmax())


    # TO DO: display most frequent combination of start station and end station trip
    df['combination'] = df['Start Station'] + ' and ' + df['End Station']
    print(' The most combination of Start and End Station is: ', df['combination'].value_count().idxmax())


    print("\nThis took %s seconds." % (time.time() - start_time))
    print('-'*40)


def trip_duration_stats(df):
    """Displays statistics on the total and average trip duration."""

    print('\nCalculating Trip Duration...\n')
    start_time = time.time()

    # TO DO: display total travel time
    print('Total travel time is: ', df['Trip Duration'].sum(), 'secs')

    # TO DO: display mean travel time
    print('The mean/average travel time is: ', df['Trip Duration'].mean(), 'secs')


    print("\nThis took %s seconds." % (time.time() - start_time))
    print('-'*40)


def user_stats(df):
    """Displays statistics on bikeshare users."""

    print('\nCalculating User Stats...\n')
    start_time = time.time()

    # TO DO: Display counts of user types
    user_types = df['User Type'].value_counts()
    print(user_types)

    # TO DO: Display counts of gender
    if city != 'washington':
        gender_counts = df['Gender'].value_counts()
        print(gender_counts)
    else:
        print('Gender stats cannot be calculated because Gender does not appear in the dataframe')
        
    

    # TO DO: Display earliest, most recent, and most common year of birth
    if city != 'washington':
         print('The Earliest birth year is: ', df['Birth Year'].min())
    
         print('The Most recent birth year is: ', df['Birth year'].max())
    
         print('The most common birth year is: ', df['Birth year'].value_counts().idxmax())
    else:
        print('Year of Birth stats cannot be calculated because Year of Birth does not appear in the daraframe')


    print("\nThis took %s seconds." % (time.time() - start_time))
    print('-'*40)

def raw_data (df):
    view_data = input("Would you like to view 5 rows of individual trip data? Enter yes or no?")
    start_loc = 0
    while (view_data != 'no'):
     print(df.iloc[start_loc:start_loc + 5])
     start_loc += 5
     view_display = input("Do you wish to continue?: ").lower()



def main():
    while True:
        city, month, day = get_filters()
        df = load_data(city, month, day)

        time_stats(df)
        station_stats(df)
        trip_duration_stats(df)
        user_stats(df)

        restart = input('\nWould you like to restart? Enter yes or no.\n')
        if restart.lower() != 'yes':
            break


if __name__ == "__main__":
	main()
