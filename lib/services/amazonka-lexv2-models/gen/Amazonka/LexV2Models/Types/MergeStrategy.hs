{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.LexV2Models.Types.MergeStrategy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.MergeStrategy
  ( MergeStrategy
      ( ..,
        MergeStrategy_Append,
        MergeStrategy_FailOnConflict,
        MergeStrategy_Overwrite
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype MergeStrategy = MergeStrategy'
  { fromMergeStrategy ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern MergeStrategy_Append :: MergeStrategy
pattern MergeStrategy_Append = MergeStrategy' "Append"

pattern MergeStrategy_FailOnConflict :: MergeStrategy
pattern MergeStrategy_FailOnConflict = MergeStrategy' "FailOnConflict"

pattern MergeStrategy_Overwrite :: MergeStrategy
pattern MergeStrategy_Overwrite = MergeStrategy' "Overwrite"

{-# COMPLETE
  MergeStrategy_Append,
  MergeStrategy_FailOnConflict,
  MergeStrategy_Overwrite,
  MergeStrategy'
  #-}
