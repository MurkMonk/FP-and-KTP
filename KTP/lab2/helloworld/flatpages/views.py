#coding:utf-8  
from django.shortcuts import render 
def home(request):    
     return render(request, 'static_handler.html', {})

# Create your views here.
