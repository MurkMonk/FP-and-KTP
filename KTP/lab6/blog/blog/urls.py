from django.conf.urls import patterns, include, url

# Uncomment the next two lines to enable the admin:
from django.contrib import admin
admin.autodiscover()

urlpatterns = patterns('',
                       url(r'^article/$', 'articles.views.archive', name = 'article'),
                       url(r'^article/(?P<article_id>\d+)$',
                           'articles.views.get_article',
                           name='get_article'
                          ),
                       url(r'^article/new/$', 'articles.views.create_post'),
                       url(r'^auth/$', 'articles.views.create_account'),
                       url(r'^login/$', 'articles.views.user_login'),
                       url(r'logout/$', 'articles.views.user_logout'),
                       url(r'^admin/', include(admin.site.urls)),
)
