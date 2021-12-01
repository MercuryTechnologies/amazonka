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
-- Module      : Amazonka.MediaTailor.Types.SpliceInsertMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.SpliceInsertMessage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Splice insert message configuration.
--
-- /See:/ 'newSpliceInsertMessage' smart constructor.
data SpliceInsertMessage = SpliceInsertMessage'
  { -- | This is written to splice_insert.avail_num, as defined in section
    -- 9.7.3.1 of the SCTE-35 specification. The default value is 0. Values
    -- must be between 0 and 256, inclusive.
    availNum :: Prelude.Maybe Prelude.Int,
    -- | This is written to splice_insert.unique_program_id, as defined in
    -- section 9.7.3.1 of the SCTE-35 specification. The default value is 0.
    -- Values must be between 0 and 256, inclusive.
    uniqueProgramId :: Prelude.Maybe Prelude.Int,
    -- | This is written to splice_insert.avails_expected, as defined in section
    -- 9.7.3.1 of the SCTE-35 specification. The default value is 0. Values
    -- must be between 0 and 256, inclusive.
    availsExpected :: Prelude.Maybe Prelude.Int,
    -- | This is written to splice_insert.splice_event_id, as defined in section
    -- 9.7.3.1 of the SCTE-35 specification. The default value is 1.
    spliceEventId :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SpliceInsertMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availNum', 'spliceInsertMessage_availNum' - This is written to splice_insert.avail_num, as defined in section
-- 9.7.3.1 of the SCTE-35 specification. The default value is 0. Values
-- must be between 0 and 256, inclusive.
--
-- 'uniqueProgramId', 'spliceInsertMessage_uniqueProgramId' - This is written to splice_insert.unique_program_id, as defined in
-- section 9.7.3.1 of the SCTE-35 specification. The default value is 0.
-- Values must be between 0 and 256, inclusive.
--
-- 'availsExpected', 'spliceInsertMessage_availsExpected' - This is written to splice_insert.avails_expected, as defined in section
-- 9.7.3.1 of the SCTE-35 specification. The default value is 0. Values
-- must be between 0 and 256, inclusive.
--
-- 'spliceEventId', 'spliceInsertMessage_spliceEventId' - This is written to splice_insert.splice_event_id, as defined in section
-- 9.7.3.1 of the SCTE-35 specification. The default value is 1.
newSpliceInsertMessage ::
  SpliceInsertMessage
newSpliceInsertMessage =
  SpliceInsertMessage'
    { availNum = Prelude.Nothing,
      uniqueProgramId = Prelude.Nothing,
      availsExpected = Prelude.Nothing,
      spliceEventId = Prelude.Nothing
    }

-- | This is written to splice_insert.avail_num, as defined in section
-- 9.7.3.1 of the SCTE-35 specification. The default value is 0. Values
-- must be between 0 and 256, inclusive.
spliceInsertMessage_availNum :: Lens.Lens' SpliceInsertMessage (Prelude.Maybe Prelude.Int)
spliceInsertMessage_availNum = Lens.lens (\SpliceInsertMessage' {availNum} -> availNum) (\s@SpliceInsertMessage' {} a -> s {availNum = a} :: SpliceInsertMessage)

-- | This is written to splice_insert.unique_program_id, as defined in
-- section 9.7.3.1 of the SCTE-35 specification. The default value is 0.
-- Values must be between 0 and 256, inclusive.
spliceInsertMessage_uniqueProgramId :: Lens.Lens' SpliceInsertMessage (Prelude.Maybe Prelude.Int)
spliceInsertMessage_uniqueProgramId = Lens.lens (\SpliceInsertMessage' {uniqueProgramId} -> uniqueProgramId) (\s@SpliceInsertMessage' {} a -> s {uniqueProgramId = a} :: SpliceInsertMessage)

-- | This is written to splice_insert.avails_expected, as defined in section
-- 9.7.3.1 of the SCTE-35 specification. The default value is 0. Values
-- must be between 0 and 256, inclusive.
spliceInsertMessage_availsExpected :: Lens.Lens' SpliceInsertMessage (Prelude.Maybe Prelude.Int)
spliceInsertMessage_availsExpected = Lens.lens (\SpliceInsertMessage' {availsExpected} -> availsExpected) (\s@SpliceInsertMessage' {} a -> s {availsExpected = a} :: SpliceInsertMessage)

-- | This is written to splice_insert.splice_event_id, as defined in section
-- 9.7.3.1 of the SCTE-35 specification. The default value is 1.
spliceInsertMessage_spliceEventId :: Lens.Lens' SpliceInsertMessage (Prelude.Maybe Prelude.Int)
spliceInsertMessage_spliceEventId = Lens.lens (\SpliceInsertMessage' {spliceEventId} -> spliceEventId) (\s@SpliceInsertMessage' {} a -> s {spliceEventId = a} :: SpliceInsertMessage)

instance Core.FromJSON SpliceInsertMessage where
  parseJSON =
    Core.withObject
      "SpliceInsertMessage"
      ( \x ->
          SpliceInsertMessage'
            Prelude.<$> (x Core..:? "AvailNum")
            Prelude.<*> (x Core..:? "UniqueProgramId")
            Prelude.<*> (x Core..:? "AvailsExpected")
            Prelude.<*> (x Core..:? "SpliceEventId")
      )

instance Prelude.Hashable SpliceInsertMessage where
  hashWithSalt salt' SpliceInsertMessage' {..} =
    salt' `Prelude.hashWithSalt` spliceEventId
      `Prelude.hashWithSalt` availsExpected
      `Prelude.hashWithSalt` uniqueProgramId
      `Prelude.hashWithSalt` availNum

instance Prelude.NFData SpliceInsertMessage where
  rnf SpliceInsertMessage' {..} =
    Prelude.rnf availNum
      `Prelude.seq` Prelude.rnf spliceEventId
      `Prelude.seq` Prelude.rnf availsExpected
      `Prelude.seq` Prelude.rnf uniqueProgramId

instance Core.ToJSON SpliceInsertMessage where
  toJSON SpliceInsertMessage' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AvailNum" Core..=) Prelude.<$> availNum,
            ("UniqueProgramId" Core..=)
              Prelude.<$> uniqueProgramId,
            ("AvailsExpected" Core..=)
              Prelude.<$> availsExpected,
            ("SpliceEventId" Core..=) Prelude.<$> spliceEventId
          ]
      )
