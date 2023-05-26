source("project_2_1.R")

data = load_page_data()
titles = get_promoted_projects(data)
show_projects(titles)