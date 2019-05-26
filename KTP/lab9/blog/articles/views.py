# Create your views here.
# coding:utf-8
from models import Article
from django.contrib.auth.models import User
from django.shortcuts import render, redirect
from django.http import Http404
from django.contrib.auth import authenticate
from django.contrib.auth import login


def archive(request):
    return render(request, 'archive.html', {"posts":Article.objects.all()})

def input_user(request):
    if request.method == "POST":
        form = {
            'username': request.POST["username"],
            'password': request.POST["password"]
        }
        if form["username"] and form["password"]:
            user = authenticate(username=form["username"],password=form["password"])
            if user is None:
                form['errors'] = u"Такой пользователь не зарегестрирован!"
                return render(request, 'auth.html', {'form': form})
            else:
                login(request, user)
            return redirect('archive')
        else:
            form['errors'] = u"Не все поля заполнены"
            return render(request, 'auth.html', {'form': form})
    else:
        return render(request, 'auth.html', {})

def get_article(request, article_id):
    try:
        post = Article.objects.get(id=article_id)
        return render(request, 'article.html', {"post": post})
    except Article.DoesNotExist:
         raise Http404


def create_post(request):
    if not request.user.is_anonymous():
        if request.method == "POST":
            form = {
                'text': request.POST["text"],
                'title': request.POST["title"]
            }
            art = None
            try:
                 art = Article.objects.get(title=form["title"])
                 print (u"Такое название уже есть!")
            except Article.DoesNotExist:
                 print (u"Совпадений по названию не найдено.")
            if form["text"] and form["title"] and art is None:
                art = Article.objects.create(text=form["text"],
                                        title=form["title"],
                                        author=request.user)
                return redirect('get_article', article_id=art.id)
            else:
                if art is not None:
                    form['errors'] = u"Имя статьи не уникально"
                else:
                    form['errors'] = u"Не все поля заполнены"
                return render(request, 'create_post.html', {'form': form})
        else:
            return render(request, 'create_post.html', {})

    else:
        raise Http404
def create_user(request):
    if request.method == "POST":
        form = {
            'username': request.POST["username"],
            'mail': request.POST["mail"],
            'password': request.POST["password"]
        }
        art = None
        try:
            art = User.objects.get(username=form["username"])
            art = User.objects.get(email=form["mail"])
            # если юзер существует, то ошибки не произойдет и
            # программа удачно доберется до следующей строчки
            print (u"Такой юзер уже есть")
        except User.DoesNotExist:
            print (u"Такого юзера ещё нет")
        if form["username"] and form["mail"] and form["password"] and art is None:
            art = User.objects.create(username=form["username"],
                                         email=form["mail"],
                                         password=form["password"])
            return redirect('archive')
        else:
            if art is not None:
                form['errors'] = u"Логин или почта уже заняты"
            else:
                form['errors'] = u"Не все поля заполнены"
            return render(request, 'registration.html', {'form': form})
    else:
        return render(request, 'registration.html', {})


