#ifndef G_DWJJICFC7TB9BZP0RXB2JZN0O1FUU
#define G_DWJJICFC7TB9BZP0RXB2JZN0O1FUU
#include <stdexcept>
#include <thread>

class ExclusiveMutex {
public:
    explicit ExclusiveMutex(std::thread::id id = std::this_thread::get_id())
        : _id{id}
    {
    }

    void lock()
    {
        this->_check();
    }

    void unlock()
    {
        this->_check();
    }

private:
    std::thread::id _id;

    void _check()
    {
        if (this->_id != std::this_thread::get_id()) {
            throw std::logic_error("ExclusiveMutex is being used "
                                   "from multiple threads");
        }
    }
};

#endif
