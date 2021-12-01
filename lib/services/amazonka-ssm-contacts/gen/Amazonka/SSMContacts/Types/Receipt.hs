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
-- Module      : Amazonka.SSMContacts.Types.Receipt
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMContacts.Types.Receipt where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMContacts.Types.ReceiptType

-- | Records events during an engagement.
--
-- /See:/ 'newReceipt' smart constructor.
data Receipt = Receipt'
  { -- | Information provided during the page acknowledgement.
    receiptInfo :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the contact channel Incident Manager
    -- engaged.
    contactChannelArn :: Prelude.Maybe Prelude.Text,
    -- | The type follows the engagement cycle, @SENT@, @DELIVERED@, and @READ@.
    receiptType :: ReceiptType,
    -- | The time receipt was @SENT@, @DELIVERED@, or @READ@.
    receiptTime :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Receipt' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'receiptInfo', 'receipt_receiptInfo' - Information provided during the page acknowledgement.
--
-- 'contactChannelArn', 'receipt_contactChannelArn' - The Amazon Resource Name (ARN) of the contact channel Incident Manager
-- engaged.
--
-- 'receiptType', 'receipt_receiptType' - The type follows the engagement cycle, @SENT@, @DELIVERED@, and @READ@.
--
-- 'receiptTime', 'receipt_receiptTime' - The time receipt was @SENT@, @DELIVERED@, or @READ@.
newReceipt ::
  -- | 'receiptType'
  ReceiptType ->
  -- | 'receiptTime'
  Prelude.UTCTime ->
  Receipt
newReceipt pReceiptType_ pReceiptTime_ =
  Receipt'
    { receiptInfo = Prelude.Nothing,
      contactChannelArn = Prelude.Nothing,
      receiptType = pReceiptType_,
      receiptTime = Core._Time Lens.# pReceiptTime_
    }

-- | Information provided during the page acknowledgement.
receipt_receiptInfo :: Lens.Lens' Receipt (Prelude.Maybe Prelude.Text)
receipt_receiptInfo = Lens.lens (\Receipt' {receiptInfo} -> receiptInfo) (\s@Receipt' {} a -> s {receiptInfo = a} :: Receipt)

-- | The Amazon Resource Name (ARN) of the contact channel Incident Manager
-- engaged.
receipt_contactChannelArn :: Lens.Lens' Receipt (Prelude.Maybe Prelude.Text)
receipt_contactChannelArn = Lens.lens (\Receipt' {contactChannelArn} -> contactChannelArn) (\s@Receipt' {} a -> s {contactChannelArn = a} :: Receipt)

-- | The type follows the engagement cycle, @SENT@, @DELIVERED@, and @READ@.
receipt_receiptType :: Lens.Lens' Receipt ReceiptType
receipt_receiptType = Lens.lens (\Receipt' {receiptType} -> receiptType) (\s@Receipt' {} a -> s {receiptType = a} :: Receipt)

-- | The time receipt was @SENT@, @DELIVERED@, or @READ@.
receipt_receiptTime :: Lens.Lens' Receipt Prelude.UTCTime
receipt_receiptTime = Lens.lens (\Receipt' {receiptTime} -> receiptTime) (\s@Receipt' {} a -> s {receiptTime = a} :: Receipt) Prelude.. Core._Time

instance Core.FromJSON Receipt where
  parseJSON =
    Core.withObject
      "Receipt"
      ( \x ->
          Receipt'
            Prelude.<$> (x Core..:? "ReceiptInfo")
            Prelude.<*> (x Core..:? "ContactChannelArn")
            Prelude.<*> (x Core..: "ReceiptType")
            Prelude.<*> (x Core..: "ReceiptTime")
      )

instance Prelude.Hashable Receipt where
  hashWithSalt salt' Receipt' {..} =
    salt' `Prelude.hashWithSalt` receiptTime
      `Prelude.hashWithSalt` receiptType
      `Prelude.hashWithSalt` contactChannelArn
      `Prelude.hashWithSalt` receiptInfo

instance Prelude.NFData Receipt where
  rnf Receipt' {..} =
    Prelude.rnf receiptInfo
      `Prelude.seq` Prelude.rnf receiptTime
      `Prelude.seq` Prelude.rnf receiptType
      `Prelude.seq` Prelude.rnf contactChannelArn
