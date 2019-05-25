# -*- coding: cp1251 -*-
from models import Article
from django.shortcuts import render, redirect
from django.http import Http404
from django.contrib.auth.models import User
from django.contrib.auth import login, authenticate, logout


def archive(request):
    try:
        name = request.user.get_full_name()
    except:
        name = ''
    return render(request, 'archive.html', {"posts": Article.objects.all(), "auth": request.user.is_authenticated(),"name": name})

def get_article(request, article_id):
    try:
        post = Article.objects.get(id=article_id)
        return render(request, 'article.html', {"post": post})
    except Article.DoesNotExist:
        raise Http404

def create_post(request):
    if not request.user.is_anonymous():
        if request.method == "POST":
            # обработать данные формы, если метод POST
            form = {
                'text': request.POST["text"],
                'title': request.POST["title"]
                }
            # в словаре form будет храниться информация, введенная пользователем
            if form["text"] and form["title"]:
                # если поля заполнены без ошибок
                articles = Article.objects.all()
                for article in articles:
                    if article.title.lower() == form.get('title').lower():
                        form['errors'] = u"Имя повторяется"
                        return render(request, 'create_post.html', {'form': form})
                article = Article.objects.create(text=form["text"],
                                       title=form["title"],
                                       author=request.user)
                return redirect('get_article', article_id=article.id)
                # перейти на страницу поста
            else:
                # если введенные данные некорректны
                form['errors'] = u"Не все поля заполнены"
                return render(request, 'create_post.html', {'form': form})
        else:
            # просто вернуть страницу с формой, если метод GET
            return render(request, 'create_post.html', {})

    else:
        raise Http404

def create_account(request):
    if request.user.is_anonymous():
        if request.method == "POST":
            form = {
                'nickname': request.POST.get('nickname'),
                'first_name': request.POST.get('first_name'),
                'last_name': request.POST.get('last_name'),
                'email': request.POST.get('email'),
                'password': request.POST.get('password'),
                'confirm_password': request.POST.get('confirm_password')
                }
            if form.get('nickname') and form.get('first_name') and form.get('last_name') and form.get('email') and form.get('password') and form.get('confirm_password'):
                if form.get('password') == form.get('confirm_password'):
                    users = User.objects.all()
                    #form['errors']=str(users)
                    #return render(request, 'create_account.html', {'form': form})
                    for user in users:
                        if user.username.lower() == form.get('nickname').lower():
                            form['errors'] = u"Имя повторяется"
                            return render(request, 'create_account.html', {'form': form})
                    user = User.objects.create_user(form.get('nickname'),form.get('email'),form.get('password'))
		    user.first_name = form.get('first_name')
		    user.last_name = form.get('last_name')
		    user.save()
		    user = authenticate(username=form.get('nickname'), password=form.get('password'))
		    login(request,user)
		    return redirect('article')
                else:
                    form['errors'] = u"Пароли не совпадают"
                    return render(request, 'create_account.html', {'form': form})
            else:
                form['errors'] = u"Заполните все поля"
                return render(request, 'create_account.html', {'form': form})
            #return render(request, 'create_account.html', {})
        else:
            # просто вернуть страницу с формой, если метод GET
            return render(request, 'create_account.html', {})
    else:
        raise Http404

def user_login(request):
    if request.user.is_anonymous():
        if request.method == "POST":
            form = {
                'nickname': request.POST.get('nickname'),
                'password': request.POST.get('password')
                }
            user = authenticate(username=form.get('nickname'), password=form.get('password'))
            if user is not None:
                login(request,user)
                return redirect('article')
            else:
                form['errors']= u"Неверное имя пользователя или пароль"
                return render(request, 'login.html', {'form': form}) 
        else:
           return render(request, 'login.html', {}) 
    else:
        raise Http404

def user_logout(request):
    logout(request)
    return redirect('article')
