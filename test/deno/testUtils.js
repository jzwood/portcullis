export randomNum = () => Math.random() * 10 * (Math.random() < 0.5 ? 1 : -1)

export const generateNumArray = (maxLen = 100) => Array(Math.floor(maxLen * Math.random())).fill(0).map(randomNum)
