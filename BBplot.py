def BBplot(x, y, b=10):
    df = pd.DataFrame({'x': x, 'y': y})
    df['x_bin'] = pd.qcut(df['x'], q=b)
    median_values = df.groupby('x_bin').agg({'x': 'median', 'y': 'median'}).reset_index()
    median_values.rename(columns={'x': 'median_x', 'y': 'median_y'}, inplace=True)
    
    plt.figure(figsize=(10, 6))
    plt.scatter(median_values['median_x'], median_values['median_y'], marker='o', color='b')
    plt.xlabel('Median of x')
    plt.ylabel('Median of y')
    plt.title('BBplot of Median Values by Bin')
    plt.grid(True)
    plt.show()

x = np.random.normal(0, 1, 10)
BBplot(x, 10*x + 5*np.random.normal(0, 1, 10))
