context(".onLoad")
describe(".onLoad", {
    it("injects 'cowsay' into the options", {
        expect_false(is.null(options('cowsay')))
    })
})
