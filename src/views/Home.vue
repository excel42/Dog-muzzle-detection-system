<template>
  <div>
    <h1>현황 조회</h1>
    <input type="text" v-model="input1" />
    <button type="button" @click="getData">검색</button>
    <button type="button" @click="setData">설정</button>

    <table class="table table-bordered" v-show="tableShow">
      <tr :key="i" v-for="(d,i) in options">
        <td>{{d.v}}</td>
        <td>{{d.t}}</td>
      </tr>
    </table>
    <h5>조회 결과</h5>
    <paginated-list :list-array="pageArray" />
    <div>
    <table>
      <tr>
        <th>ID</th>
        <th>Img</th>
        <th>Dog</th>
        <th>Breed</th>
        <th>Fierce_Dog</th>
        <th>Muzzle</th>
        <th>Info</th>
        <th>Time</th>
      </tr>
      <tr v-for="item in items" :key="item.title">
        <td><span v-html="item.title"></span></td>
        <td><img v-bind:src="item.image1"></td>
        <td><img v-bind:src="item.image2"></td>
        <td><span v-html="item.subtitle"></span></td>
        <td><span v-html="item.pubDate"></span></td>
        <td><span v-html="item.director"></span></td>
        <td><span v-html="item.actor"></span></td>
        <td><span v-html="item.userRating"></span></td>
      </tr>
    </table>
  </div>
  </div>
   
</template>


<script>
  export default {
    data() {
      return {
        items: [
          {
            "title": "c001",
            "image1": "https://i.imgur.com/kvb6RSv.jpg",
            "image2": "https://i.imgur.com/xin2xM1.jpg",
            "subtitle": "boston_bull",
            "pubDate": "Y",
            "director": "Y",
            "actor": "",
            "userRating": "2021-05-13 16:05:34.078"
          },
          {
            "title": "c002</b>",
            "image1": "https://i.imgur.com/JxbgFfI.jpg",
            "image2": "https://i.imgur.com/t08FGkL.jpg",
            "subtitle": "black-and-tan_coonhound",
            "pubDate": "N",
            "director": "N",
            "actor": "",
            "userRating": "2021-05-13 16:07:28.023"
          },
          {
            "title": "c003",
            "image1": "https://i.imgur.com/7ZYtfmR.jpg",
            "image2": "https://i.imgur.com/v6uPGd3.jpg",
            "subtitle": "pitbull_terrier",
            "pubDate": "Y",
            "director": "N",
            "actor": "",
            "userRating": "2021-05-13 16:10:87.040"
          }
        ]
      }
    }
  }
</script>

<style>
td, th {
  padding: 10px;
  border: 1px solid #ccc;
}
body {
  padding: 1rem;
}
img {
	max-width: 100%;
	width: 100px;
}
</style>
