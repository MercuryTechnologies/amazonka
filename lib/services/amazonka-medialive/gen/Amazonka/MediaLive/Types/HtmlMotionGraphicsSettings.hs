{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaLive.Types.HtmlMotionGraphicsSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.HtmlMotionGraphicsSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Html Motion Graphics Settings
--
-- /See:/ 'newHtmlMotionGraphicsSettings' smart constructor.
data HtmlMotionGraphicsSettings = HtmlMotionGraphicsSettings'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HtmlMotionGraphicsSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newHtmlMotionGraphicsSettings ::
  HtmlMotionGraphicsSettings
newHtmlMotionGraphicsSettings =
  HtmlMotionGraphicsSettings'

instance Core.FromJSON HtmlMotionGraphicsSettings where
  parseJSON =
    Core.withObject
      "HtmlMotionGraphicsSettings"
      (\x -> Prelude.pure HtmlMotionGraphicsSettings')

instance Prelude.Hashable HtmlMotionGraphicsSettings where
  hashWithSalt salt' _ =
    salt' `Prelude.hashWithSalt` (0 :: Prelude.Int)

instance Prelude.NFData HtmlMotionGraphicsSettings where
  rnf _ = ()

instance Core.ToJSON HtmlMotionGraphicsSettings where
  toJSON = Prelude.const (Core.Object Prelude.mempty)
