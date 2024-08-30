static wchar_t* ConstantString(const char* data, int len)
{
    wchar_t* wc = new wchar_t[len];
    mbstowcs (wc, data, len);
    return wc;
}
