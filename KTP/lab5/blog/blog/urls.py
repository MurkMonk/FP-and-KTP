from django.conf.urls import patterns, include, url

# Uncomment the next two lines to enable the admin:
from django.contrib import admin
admin.autodiscover()

urlpatterns = patterns('',
                       url(r'^article/$', 'articles.views.archive'),
                       url(r'^article/(?P<article_id>\d+)$',
                           'articles.views.get_article',
                           name='get_article'
                          ),
                       url(r'^article/new/$', 'articles.views.create_post'),
                       url(r'^admin/', include(admin.site.urls)),
)
