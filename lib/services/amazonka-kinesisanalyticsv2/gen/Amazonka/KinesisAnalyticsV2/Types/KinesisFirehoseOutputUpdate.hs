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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.KinesisFirehoseOutputUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.KinesisFirehoseOutputUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | For a SQL-based Kinesis Data Analytics application, when updating an
-- output configuration using the UpdateApplication operation, provides
-- information about a Kinesis Data Firehose delivery stream that is
-- configured as the destination.
--
-- /See:/ 'newKinesisFirehoseOutputUpdate' smart constructor.
data KinesisFirehoseOutputUpdate = KinesisFirehoseOutputUpdate'
  { -- | The Amazon Resource Name (ARN) of the delivery stream to write to.
    resourceARNUpdate :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KinesisFirehoseOutputUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceARNUpdate', 'kinesisFirehoseOutputUpdate_resourceARNUpdate' - The Amazon Resource Name (ARN) of the delivery stream to write to.
newKinesisFirehoseOutputUpdate ::
  -- | 'resourceARNUpdate'
  Prelude.Text ->
  KinesisFirehoseOutputUpdate
newKinesisFirehoseOutputUpdate pResourceARNUpdate_ =
  KinesisFirehoseOutputUpdate'
    { resourceARNUpdate =
        pResourceARNUpdate_
    }

-- | The Amazon Resource Name (ARN) of the delivery stream to write to.
kinesisFirehoseOutputUpdate_resourceARNUpdate :: Lens.Lens' KinesisFirehoseOutputUpdate Prelude.Text
kinesisFirehoseOutputUpdate_resourceARNUpdate = Lens.lens (\KinesisFirehoseOutputUpdate' {resourceARNUpdate} -> resourceARNUpdate) (\s@KinesisFirehoseOutputUpdate' {} a -> s {resourceARNUpdate = a} :: KinesisFirehoseOutputUpdate)

instance Prelude.Hashable KinesisFirehoseOutputUpdate where
  hashWithSalt salt' KinesisFirehoseOutputUpdate' {..} =
    salt' `Prelude.hashWithSalt` resourceARNUpdate

instance Prelude.NFData KinesisFirehoseOutputUpdate where
  rnf KinesisFirehoseOutputUpdate' {..} =
    Prelude.rnf resourceARNUpdate

instance Core.ToJSON KinesisFirehoseOutputUpdate where
  toJSON KinesisFirehoseOutputUpdate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ResourceARNUpdate" Core..= resourceARNUpdate)
          ]
      )
