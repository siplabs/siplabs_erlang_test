<p>Пояснения к решению:</p>
<p> С прошлой версии программа претерпела существенные изменения, главное из которых переход на OTP.
 Теперь сервер представлен в виде gen_server. Потребление памяти сделано константным. Развёртывание серверов на указанные в .app файле
 узлах происходит автоматически (Ноды должны быть запущенны и должны находиться в каталоге с актуальной версией кода. Куки должны совпадать.). </p>