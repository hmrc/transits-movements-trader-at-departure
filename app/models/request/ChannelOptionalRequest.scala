/*
 * Copyright 2020 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package models.request

import models.ChannelType
import models.ChannelType.api
import models.ChannelType.web
import play.api.mvc.Request
import play.api.mvc.WrappedRequest

trait ChannelOptionalRequest[A] extends WrappedRequest[A] {

  def getChannel: ChannelType = ChannelUtil.getChannel(this)
}

object ChannelUtil {

  def getChannel[A](request: Request[A]): ChannelType =
    request.headers.get("channel") match {
      case Some(channel) if channel.equals(api.toString) => api
      case _                                             => web
    }
}
