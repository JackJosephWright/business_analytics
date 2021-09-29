def get_prop_table():

    #trying to get the player name in every row

    all_spans = driver.find_elements_by_xpath("//*[@class='py-2']")
    name_list = []
    for span in all_spans:
        name_list.append(span.text)

    #get prop for every row
    all_spans = driver.find_elements_by_xpath('//*[@class="g-mt-1 m-micro-copy"]')
    prop_list = []
    for span in all_spans:
        prop_list.append(span.text)
    prop_list

        
    all_spans = driver.find_elements_by_xpath("//*[@class='kyber-table-body-cell kyber-table-body-cell--align-right']")
    value=[]
    for span in all_spans:
        value.append(span.text)


    np_array=np.array(value)

    reshaped_array=np.reshape(np_array, (int((len(value)/6)),6))
    df = pd.DataFrame(reshaped_array, columns=['projected','line','odds_over','value_over_pct','odds_under','value_under_pct'])
    #drop last row
    df.drop(df.tail(1).index,inplace=True)
    df['name']=name_list
    df['prop']=prop_list
    #strip out percents
    df['value_over_pct']=df['value_over_pct'].str.rstrip("%").astype('float')/100
    df['value_under_pct']=df['value_under_pct'].str.rstrip("%").astype('float')/100
    return df